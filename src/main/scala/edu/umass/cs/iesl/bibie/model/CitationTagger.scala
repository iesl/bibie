package edu.umass.cs.iesl.bibie.model

/**
 * @author Kate Silverstein 
 *         created on 9/30/15
 */

import edu.umass.cs.iesl.bibie._
import cc.factorie.app.nlp._
import cc.factorie.app.chain._
import java.io._
import cc.factorie.util._
import java.net.URL
import edu.umass.cs.iesl.bibie.load.{IOHelper, LoadGrobid, PreFeatures}
import cc.factorie.optimize._
import java.util.logging.Logger
import edu.umass.cs.iesl.bibie.evaluate.SegmentationEvaluation
import edu.umass.cs.iesl.bibie.segment.OverSegmenter
import edu.umass.cs.iesl.bibie.util.DefaultLexicons

class CitationTagger extends DocumentAnnotator {

  private val logger = Logger.getLogger(getClass.getName)

  /* Deserialize this HeaderTagger from the model at the given URL */
  def this(url:java.net.URL) = {
    this()
    if (url != null) {
      deserialize(url.openConnection.getInputStream)
      println("Found model")
    }
    else {
      println("model not found")
    }
  }

  /* Deserialize this HeaderTagger from the model at the given path on disk */
  def this(modelPath: String) = {
    this(new URL("file://" + modelPath))
  }

  class CitationTaggerCRFModel extends ChainModel[CitationLabel, CitationFeatures, Token](
    CitationLabelDomain,
    CitationFeaturesDomain,
    l => l.token.attr[CitationFeatures],
    l => l.token,
    t => t.attr[CitationLabel]
  ) {
    def sparsity = parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat / parameters.tensors.sumInts(_.length)
  }

  val model = new CitationTaggerCRFModel
  val objective = cc.factorie.variable.HammingObjective

  var lexicons: DefaultLexicons = null

  /* DocumentAnnotator methods */
  def tokenAnnotationString(token:Token): String = s"${token.attr[CitationLabel].categoryValue}"
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[CitationLabel])

  def process(document:Document): Document = {
    document
  }

  def processBoth(document: Document): Document = {
    if (document.tokenCount == 0) return document
    if (!document.tokens.head.attr.contains(classOf[CitationFeatures])) {
      addFeaturesBoth(document, training = false)
    }
    if (document.sentenceCount > 0) {
      for (sentence <- document.sentences if sentence.tokens.nonEmpty) {
        sentence.tokens.foreach { token => if (!token.attr.contains(classOf[CitationLabel])) token.attr += new CitationLabel("O", token) }
        val vars = sentence.tokens.map(_.attr[CitationLabel]).toSeq
        model.maximize(vars)(null)
      }
    } else {
      document.tokens.foreach { token => if (!token.attr.contains(classOf[CitationLabel])) token.attr += new CitationLabel("O", token) }
      val vars = document.tokens.map(_.attr[CitationLabel]).toSeq
      model.maximize(vars)(null)
    }
    document
  }

  def processGrobid(document:Document): Document = {
    if (document.tokenCount == 0) return document
    if (!document.tokens.head.attr.contains(classOf[CitationFeatures])) {
      addFeaturesGrobid(document, training = false)
    }
    if (document.sentenceCount > 0) {
      for (sentence <- document.sentences if sentence.tokens.size > 0) {
        sentence.tokens.foreach { token => if (!token.attr.contains(classOf[CitationLabel])) token.attr += new CitationLabel("O", token) }
        val vars = sentence.tokens.map(_.attr[CitationLabel]).toSeq
        model.maximize(vars)(null)
      }
    } else {
      document.tokens.foreach { token => if (!token.attr.contains(classOf[CitationLabel])) token.attr += new CitationLabel("O", token) }
      val vars = document.tokens.map(_.attr[CitationLabel]).toSeq
      model.maximize(vars)(null)
    }
    document
  }

  def addFeatures(doc: Document, training: Boolean = false): Unit = {
    assert(lexicons != null)
    val featureBuilder = new Features(lexicons)
    if (training) {
      val sentenceIter = doc.sentences.toIterator
      while (sentenceIter.hasNext) {
        val sentence = sentenceIter.next()
        if (sentence.nonEmpty) {
          sentence.tokens.foreach { token =>
            if (token.attr[CitationFeatures] == null) token.attr += new CitationFeatures(token)
            featureBuilder.wordToFeatures(token)
          }
        }
      }
    } else {
      doc.sentences.filter(_.nonEmpty).par.foreach { sentence =>
        sentence.tokens.foreach { token =>
          if (token.attr[CitationFeatures] == null) token.attr += new CitationFeatures(token)
          featureBuilder.wordToFeatures(token)
        }
      }
    }
    featureBuilder.initSentenceFeatures(doc)
  }

  def addFeaturesBoth(doc: Document, training: Boolean = false): Unit = {
    computeDocumentFeaturesGrobid(doc, training = training)
    addFeatures(doc, training = training)
  }

  def addFeaturesGrobid(doc: Document, training: Boolean = false): Unit = {
    computeDocumentFeaturesGrobid(doc, training)
  }

  def computeDocumentFeaturesGrobid(doc: Document, training: Boolean): Unit = {
    if (training) {
      val sentenceIter = doc.sentences.toIterator
      while (sentenceIter.hasNext) {
        val sentence = sentenceIter.next()
        if (sentence.nonEmpty) {
          computeTokenFeaturesGrobid(sentence)
        }
      }
    } else {
      doc.sentences.filter(_.nonEmpty).par.foreach { sentence =>
        computeTokenFeaturesGrobid(sentence)
      }
    }
  }

  def computeTokenFeaturesGrobid(sentence: Sentence): Unit = {
    sentence.tokens.foreach { token =>
      val features = new CitationFeatures(token)
      if (token.attr[PreFeatures] != null) {
        features ++= token.attr[PreFeatures].features
      }
      token.attr += features
    }
  }

  def trainUsingBothFeatures(trainDocuments: Seq[Document], testDocuments: Seq[Document], params: Hyperparams)(implicit random: scala.util.Random): Double = {
    lexicons = new DefaultLexicons(params.lexiconUrl)

    // generate features
    trainDocuments.foreach(d => addFeaturesBoth(d, training = true))
    println(s"feature domain size: ${CitationFeaturesDomain.dimensionDomain.size}")
    CitationFeaturesDomain.freeze()
    testDocuments.foreach(d => addFeaturesBoth(d, training = false))

    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels: Seq[CitationLabel] = trainDocuments.flatMap(_.tokens).map(_.attr[CitationLabel])
    val testLabels: Seq[CitationLabel] = testDocuments.flatMap(_.tokens).map(_.attr[CitationLabel])
    (trainLabels ++ testLabels).filter(_ != null).foreach(_.setRandomly(random))

    val vars = for (td <- trainDocuments; sentence <- td.sentences if sentence.length > 1) yield sentence.tokens.map(_.attr[CitationLabel])
    val examples = vars.map(v => new LikelihoodExample(v.toSeq, model, cc.factorie.infer.InferByBPChain))
    val evaluator = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)

    def evaluate(): Unit = {
      trainDocuments.foreach(processBoth)
      testDocuments.foreach(processBoth)
      evaluator.printEvaluation(trainDocuments, extra = "TRAIN")
      evaluator.segmentationEvaluation(trainDocuments, extra = "TRAIN")
      evaluator.printEvaluation(testDocuments, extra = "DEV")
      evaluator.segmentationEvaluation(testDocuments, extra = "DEV")
    }

    params.optimizer match {
      case "lbfgs" =>
        val optimizer = new LBFGS with L2Regularization
        val trainer = new ThreadLocalBatchTrainer(model.parameters, optimizer)
        logger.info("training with LBFGS ...")
        trainer.trainFromExamples(examples)
      case "adagrad" =>
        val optimizer = new AdaGradRDA(delta=params.delta, rate=params.rate, l1=params.l1, l2=params.l2, numExamples=examples.length)
        logger.info("training with AdaGradRDA ...")
        Trainer.onlineTrain(model.parameters, examples, evaluate=evaluate, useParallelTrainer=false, maxIterations=params.numIterations, optimizer=optimizer)
      case _ => throw new Exception(s"invalid optimizer: ${params.optimizer}")
    }

    (trainLabels ++ testLabels).foreach(_.setRandomly(random))
    trainDocuments.foreach(processBoth)
    testDocuments.foreach(processBoth)
    logger.info(s"model sparsity: ${model.sparsity}")

    evaluator.segmentationEvaluation(trainDocuments, extra = "TRAIN")
    evaluator.printEvaluation(trainDocuments, extra = "TRAIN")
    evaluator.segmentationEvaluation(testDocuments, extra = "DEV")
    evaluator.printEvaluation(testDocuments, extra = "DEV")
  }

  def trainUsingGrobidFeatures(trainDocuments: Seq[Document], testDocuments: Seq[Document], params: Hyperparams)(implicit random: scala.util.Random): Double = {
    // generate features
    trainDocuments.foreach(d => addFeaturesGrobid(d, training = true))
    println(s"feature domain size: ${CitationFeaturesDomain.dimensionDomain.size}")
    CitationFeaturesDomain.freeze()
    testDocuments.foreach(d => addFeaturesGrobid(d, training = false))

    val trainLabels: Seq[CitationLabel] = trainDocuments.flatMap(_.tokens).map(_.attr[CitationLabel])
    val testLabels: Seq[CitationLabel] = testDocuments.flatMap(_.tokens).map(_.attr[CitationLabel])
    (trainLabels ++ testLabels).filter(_ != null).foreach(_.setRandomly(random))
    val vars = for (td <- trainDocuments; sentence <- td.sentences if sentence.length > 1) yield sentence.tokens.map(_.attr[CitationLabel])
    val examples = vars.map(v => new LikelihoodExample(v.toSeq, model, cc.factorie.infer.InferByBPChain))
    val evaluator = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)

    def evaluate(): Unit = {
      trainDocuments.foreach(processGrobid)
      testDocuments.foreach(processGrobid)
      evaluator.printEvaluation(trainDocuments, extra = "TRAIN")
      evaluator.segmentationEvaluation(trainDocuments, extra = "TRAIN")
      evaluator.printEvaluation(testDocuments, extra = "DEV")
      evaluator.segmentationEvaluation(testDocuments, extra = "DEV")
    }

    params.optimizer match {
      case "lbfgs" =>
        val optimizer = new LBFGS with L2Regularization
        val trainer = new ThreadLocalBatchTrainer(model.parameters, optimizer)
        logger.info("training with LBFGS ...")
        trainer.trainFromExamples(examples)
      case "adagrad" =>
        val optimizer = new AdaGradRDA(delta=params.delta, rate=params.rate, l1=params.l1, l2=params.l2, numExamples=examples.length)
        logger.info("training with AdaGradRDA ...")
        Trainer.onlineTrain(model.parameters, examples, evaluate=evaluate, useParallelTrainer=false, maxIterations=params.numIterations, optimizer=optimizer)
      case _ => throw new Exception(s"invalid optimizer: ${params.optimizer}")
    }

    (trainLabels ++ testLabels).foreach(_.setRandomly(random))
    trainDocuments.foreach(processGrobid)
    testDocuments.foreach(processGrobid)
    logger.info(s"model sparsity: ${model.sparsity}")
    evaluator.segmentationEvaluation(trainDocuments, extra = "TRAIN")
    evaluator.printEvaluation(trainDocuments, extra = "TRAIN")
    evaluator.segmentationEvaluation(testDocuments, extra = "DEV")
    evaluator.printEvaluation(testDocuments, extra = "DEV")
  }

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(CitationLabelDomain, is)
    BinarySerializer.serialize(CitationFeaturesDomain.dimensionDomain, is)
    BinarySerializer.serialize(model, is)
    is.close()
  }

  def deserialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(CitationLabelDomain, is)
    CitationLabelDomain.freeze()
    BinarySerializer.deserialize(CitationFeaturesDomain.dimensionDomain, is)
    CitationFeaturesDomain.freeze()
    println(s"feature domain size: ${CitationFeaturesDomain.dimensionDomain.size}")
    BinarySerializer.deserialize(model, is)
    println(s"model sparsity: ${model.sparsity}")
    is.close()
  }
}

object TrainCitationTagger extends HyperparameterMain {
  private val logger = Logger.getLogger(getClass.getName)
  def evaluateParameters(args: Array[String]): Double = {
    val opts = new BibieOptions
    opts.parse(args)
    println(opts.unParse.mkString(" "))
    val id = ExperimentId(opts.dataSet.value, opts.featureSet.value)
    id match {
      case ExperimentId(DATA_GROBID, FEATURES_GROBID) => trainWithGrobidFeatures(opts)
      case ExperimentId(DATA_GROBID, FEATURES_BOTH) => trainWithBothFeatures(opts)
      case _ => throw new Exception(s"bad dataSet or featureSet option: ${opts.dataSet.value}, ${opts.featureSet.value}")
    }
  }

  def trainWithGrobidFeatures(opts: BibieOptions): Double = {
    logger.info(" ** trainModelGrobidFeaturesOnly **")
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val lexiconDir = opts.lexiconUrl.value
    var tagger = new CitationTagger
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value)
    val testDocs = LoadGrobid.fromFilename(opts.testFile.value)
    var result = 0.0

    if (opts.useCrossValidation.value) {

      val k = opts.nFolds.value
      val runs = Array.fill(k)(0.0)
      val models: Array[CitationTagger] = Array.fill(k)(null)
      val data: Seq[Document] = scala.util.Random.shuffle(trainDocs ++ testDocs)
      val folds = data.grouped(k).toSeq

      var i = 0
      while (i < k) {
        logger.info(s"on fold $i out of $k")
        val validation = folds(i)
        val training: Seq[Document] = (folds.take(i-1) ++ folds.drop(i)).flatten
        CitationFeaturesDomain.dimensionDomain.unfreeze()
        CitationFeaturesDomain.dimensionDomain.clear()
        val tg = new CitationTagger
        val f1 = tg.trainUsingGrobidFeatures(training, validation, params)
        runs(i) = f1
        models(i) = tg
        i += 1
      }

      val avg = runs.sum / k.toDouble
      logger.info(s"avg f1: $avg")
      val best = runs.zip(models).maxBy { case (f1, model) => f1 }
      result = best._1
      tagger = best._2

    } else {
      val trainEval = tagger.trainUsingGrobidFeatures(trainDocs, testDocs, params)
      result = trainEval
      logger.info(s"train eval: $trainEval")
    }

    if (opts.saveModel.value) {
      logger.info(s"serializing model to ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }

    //val fname = "/home/kate/AI2/bibie/grobid-results.txt"
    val fname = "/iesl/canvas/ksilvers/bibie-exec/grobid-results.txt"
    val evalDocs = LoadGrobid.fromFilenameWithGrobidResults(fname)
    evalDocs.foreach { doc =>
      doc.tokens.foreach { token =>
        val label = token.attr[CitationLabel]
        token.attr.remove[CitationLabel]
        val newLabel = new CitationLabel(label.target.categoryValue, token)
        token.attr += newLabel
      }
    }

    evalDocs.foreach(tagger.processGrobid)
    val eval = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)
    eval.printEvaluation(evalDocs, "UMASS")
    eval.segmentationEvaluation(evalDocs, "UMASS (SegmentationEvaluation)")

    result
  }

  def trainWithBothFeatures(opts: BibieOptions): Double = {
    logger.info(" ** train model using both features **")
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val lexiconDir = opts.lexiconUrl.value
    val tagger = new CitationTagger
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value)
    val testDocs = LoadGrobid.fromFilename(opts.testFile.value)
    OverSegmenter.overSegment(trainDocs ++ testDocs, lexiconDir)
    val trainEval = tagger.trainUsingBothFeatures(trainDocs, testDocs, params)
    logger.info(s"train eval: $trainEval")
    if (opts.saveModel.value) {
      logger.info(s"serializing model to ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }

    //val fname = "/home/kate/AI2/bibie/grobid-results.txt"
    val fname = "/iesl/canvas/ksilvers/bibie-exec/grobid-results.txt"
    val evalDocs = LoadGrobid.fromFilenameWithGrobidResults(fname)
    OverSegmenter.overSegment(evalDocs, lexiconDir)
    evalDocs.foreach { doc =>
      doc.tokens.foreach { token =>
        val label = token.attr[CitationLabel]
        token.attr.remove[CitationLabel]
        val newLabel = new CitationLabel(label.target.categoryValue, token)
        token.attr += newLabel
      }
    }

    evalDocs.foreach(tagger.processBoth)
    val eval = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)
    eval.printEvaluation(evalDocs, "UMASS")
    eval.segmentationEvaluation(evalDocs, "UMASS (SegmentationEvaluation)")

    trainEval
  }
}

object OptimizeCitationTagger {
  def main(args: Array[String]): Unit = {
    println(args.mkString(", "))
    val opts = new BibieOptions
    opts.parse(args)
    opts.saveModel.setValue(false)
    //    opts.writeEvals.setValue(false)
    val l1 = HyperParameter(opts.l1, new LogUniformDoubleSampler(1e-6, 1))
    val l2 = HyperParameter(opts.l2, new LogUniformDoubleSampler(1e-6, 1))
    val rate = HyperParameter(opts.rate, new LogUniformDoubleSampler(1e-4, 1))
    val delta = HyperParameter(opts.delta, new LogUniformDoubleSampler(1e-4, 1))
    val qs = new QSubExecutor(10, "edu.umass.cs.iesl.bibie.model.TrainCitationTagger")
    val optimizer = new HyperParameterSearcher(opts, Seq(l1, l2, rate, delta), qs.execute, 200, 180, 60)
    val result = optimizer.optimize()
    println("Got results: " + result.mkString(" "))
    println("Best l1: " + opts.l1.value + " best l2: " + opts.l2.value)
    println("Running best configuration...")
    opts.saveModel.setValue(true)
    //    opts.writeEvals.setValue(true)
    import scala.concurrent.Await
    import scala.concurrent.duration._
    Await.result(qs.execute(opts.values.flatMap(_.unParse).toArray), 1.hours)
    println("Done.")
  }
}
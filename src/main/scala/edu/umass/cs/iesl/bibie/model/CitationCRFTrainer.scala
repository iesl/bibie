package edu.umass.cs.iesl.bibie.model

import java.util.logging.Logger

import cc.factorie.app.nlp.{Document, Sentence, Token}
import cc.factorie.optimize.{AdaGradRDA, L2Regularization, LBFGS, LikelihoodExample, ThreadLocalBatchTrainer, Trainer}
import cc.factorie.util._
import cc.factorie.{random, _}

import edu.umass.cs.iesl.bibie._
import edu.umass.cs.iesl.bibie.evaluate.SegmentationEvaluation
import edu.umass.cs.iesl.bibie.load.{LoadHier, PreFeatures, IOHelper, LoadGrobid}
import edu.umass.cs.iesl.bibie.segment._
import edu.umass.cs.iesl.bibie.util.{DefaultLexicons, Lexicons}

class CitationCRFTrainer(val lexicons: Lexicons) {

//  /* Deserialize this CitationCRFTagger from the model at the given URL */
//  def this(url:java.net.URL, lexicons: Lexicons) = {
//    this(lexicons)
//    if (url != null) {
//      deserialize(url.openConnection.getInputStream)
//      println("Found model")
//    }
//    else {
//      println("model not found")
//    }
//  }
//
//  /* Deserialize this CitationCRFTagger from the model at the given path on disk */
//  def this(modelPath: String, lexicons: Lexicons) = {
//    this(new URL("file://" + modelPath), lexicons)
//  }

  private val logger = Logger.getLogger(getClass.getName)

  val evaluator = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)
  val model = new CitationCRFModel(lexicons)
//  val featureBuilder = new Features(lexicons)
//
//  def wordToFeatures(token: Token): Unit = featureBuilder.wordToFeatures(token)
//  def initSentenceFeatures(d: Document): Unit = featureBuilder.initSentenceFeatures(d)
//  def initSentenceFeatures(data: Seq[Document]): Unit = featureBuilder.initSentenceFeatures(data)

//  def tag(token: Token): String = token.attr[CitationLabel].categoryValue.substring(2)

//  def test(testDocuments: Seq[Document]) {
////    for (d <- testDocuments) {
////      d.tokens.foreach(t => wordToFeatures(t))
////    }
////    initSentenceFeatures(testDocuments)
//    testDocuments.foreach { doc => model.computeDocumentFeatures(doc, training = false) }
//    val testLabels: Seq[CitationLabel] = testDocuments.flatMap(_.tokens).map(_.attr[CitationLabel]).toSeq
//    testLabels.foreach(_.setRandomly(random))
//    testDocuments.foreach(process)
//    evaluator.printEvaluation(testDocuments, testDocuments, "FINAL")
//    evaluator.segmentationEvaluation(testDocuments, testDocuments, "FINAL")
//    println("Test Documents")
//  }

  def train(trainDocuments: Seq[Document], testDocuments: Seq[Document], params: Hyperparams)(implicit random: scala.util.Random): Double = {
    // generate features
    computeDocumentsFeatures(trainDocuments, training = true)
    println(s"feature domain size: ${CitationFeaturesDomain.dimensionDomain.size}")
    CitationFeaturesDomain.freeze()
    computeDocumentsFeatures(testDocuments, training = false)

    // todo feature count cutoff?

    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels: Seq[CitationLabel] = trainDocuments.flatMap(_.tokens).map(_.attr[CitationLabel])
    val testLabels: Seq[CitationLabel] = testDocuments.flatMap(_.tokens).map(_.attr[CitationLabel])
    (trainLabels ++ testLabels).filter(_ != null).foreach(_.setRandomly(random))
    val vars = for (td <- trainDocuments; sentence <- td.sentences if sentence.length > 1) yield sentence.tokens.map(_.attr[CitationLabel])
    val examples = vars.map(v => new LikelihoodExample(v.toSeq, model, cc.factorie.infer.InferByBPChain))
    val annotator = new BibieAnnotator(model, params.lexiconUrl)

    def evaluate(): Unit = {
      trainDocuments.foreach(annotator.processDuringTraining)
      testDocuments.foreach(annotator.processDuringTraining)
      model.evaluate(trainDocuments, "TRAINING")
      model.evaluate(testDocuments, "DEV")
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
    trainDocuments.foreach(annotator.process)
    testDocuments.foreach(annotator.process)
    val tot = model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat
    val len = model.parameters.tensors.sumInts(_.length)
    val sparsity = tot / len
    logger.info(s"model sparsity: $sparsity")
    model.evaluate(trainDocuments, "TRAINING (FINAL)")
    model.evaluate(testDocuments, "DEV (FINAL)")
  }

  def trainUsingGrobidFeatures(trainDocuments: Seq[Document], testDocuments: Seq[Document], params: Hyperparams)(implicit random: scala.util.Random): Double = {
    // generate features
    computeDocumentsFeaturesGrobid(trainDocuments, training = true)
    println(s"feature domain size: ${CitationFeaturesDomain.dimensionDomain.size}")
    CitationFeaturesDomain.freeze()
    computeDocumentsFeaturesGrobid(testDocuments, training = false)

    // todo feature count cutoff?

    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels: Seq[CitationLabel] = trainDocuments.flatMap(_.tokens).map(_.attr[CitationLabel])
    val testLabels: Seq[CitationLabel] = testDocuments.flatMap(_.tokens).map(_.attr[CitationLabel])
    (trainLabels ++ testLabels).filter(_ != null).foreach(_.setRandomly(random))
    val vars = for (td <- trainDocuments; sentence <- td.sentences if sentence.length > 1) yield sentence.tokens.map(_.attr[CitationLabel])
    val examples = vars.map(v => new LikelihoodExample(v.toSeq, model, cc.factorie.infer.InferByBPChain))
    val annotator = new BibieAnnotator(model, params.lexiconUrl)

    def evaluate(): Unit = {
      trainDocuments.foreach(annotator.processGrobid)
      testDocuments.foreach(annotator.processGrobid)
      model.evaluate(trainDocuments, "TRAINING")
      model.evaluate(testDocuments, "DEV")
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
    trainDocuments.foreach(annotator.processGrobid)
    testDocuments.foreach(annotator.processGrobid)
    val tot = model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat
    val len = model.parameters.tensors.sumInts(_.length)
    val sparsity = tot / len
    logger.info(s"model sparsity: $sparsity")
    model.evaluate(trainDocuments, "TRAINING (FINAL)")
    model.evaluate(testDocuments, "DEV (FINAL)")
  }

  def trainUsingBothFeatureSets(trainDocuments: Seq[Document], testDocuments: Seq[Document], params: Hyperparams)(implicit random: scala.util.Random): Double = {
    // generate features
    computeDocumentsFeaturesBoth(trainDocuments, training = true)
    println(s"feature domain size: ${CitationFeaturesDomain.dimensionDomain.size}")
    CitationFeaturesDomain.freeze()
    computeDocumentsFeaturesBoth(testDocuments, training = false)

    // todo feature count cutoff?

    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels: Seq[CitationLabel] = trainDocuments.flatMap(_.tokens).map(_.attr[CitationLabel])
    val testLabels: Seq[CitationLabel] = testDocuments.flatMap(_.tokens).map(_.attr[CitationLabel])
    (trainLabels ++ testLabels).filter(_ != null).foreach(_.setRandomly(random))
    val vars = for (td <- trainDocuments; sentence <- td.sentences if sentence.length > 1) yield sentence.tokens.map(_.attr[CitationLabel])
    val examples = vars.map(v => new LikelihoodExample(v.toSeq, model, cc.factorie.infer.InferByBPChain))
    val annotator = new BibieAnnotator(model, params.lexiconUrl)

    def evaluate(): Unit = {
      trainDocuments.foreach(annotator.processBoth)
      testDocuments.foreach(annotator.processBoth)
      model.evaluate(trainDocuments, "TRAINING")
      model.evaluate(testDocuments, "DEV")
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
    trainDocuments.foreach(annotator.processBoth)
    testDocuments.foreach(annotator.processBoth)
    val tot = model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat
    val len = model.parameters.tensors.sumInts(_.length)
    val sparsity = tot / len
    logger.info(s"model sparsity: $sparsity")
    model.evaluate(trainDocuments, "TRAINING (FINAL)")
    model.evaluate(testDocuments, "DEV (FINAL)")
  }

//  /* Assumes features are already computed */
//  def predict(document: Document): Document = {
//    for (sentence <- document.sentences if sentence.tokens.size > 0) {
//      val vars = sentence.tokens.map(_.attr[CitationLabel]).toSeq
//      val sum = CitationBIOHelper.infer(vars, model)
//      sum.setToMaximize(null)
//    }
//    document
//  }

  def computeDocumentsFeatures(documents: Seq[Document], training: Boolean = false) = {
    if (training) {
      var i = 0
      while (i < documents.length) {
        model.computeDocumentFeatures(documents(i), training = true)
        i += 1
      }
    } else {
      documents.par.foreach { document =>
        model.computeDocumentFeatures(document, training = false)
      }
    }
  }

  def computeDocumentsFeaturesGrobid(documents: Seq[Document], training: Boolean = false) = {
    if (training) {
      var i = 0
      while (i < documents.length) {
        model.computeDocumentFeaturesGrobid(documents(i), training = true)
        i += 1
      }
    } else {
      documents.par.foreach { document =>
        model.computeDocumentFeaturesGrobid(document, training = false)
      }
    }
  }

  def computeDocumentsFeaturesBoth(documents: Seq[Document], training: Boolean = false) = {
    if (training) {
      var i = 0
      while (i < documents.length) {
        model.computeDocumentFeaturesBoth(documents(i), training = true)
        i += 1
      }
    } else {
      documents.par.foreach { document =>
        model.computeDocumentFeaturesBoth(document, training = false)
      }
    }
  }


  //  def computeDocumentFeatures(document: Document) = {
//    for (sentence <- document.sentences if sentence.tokens.size > 0) computeTokenFeatures(sentence)
//    initSentenceFeatures(document)
//  }
//
//  def computeTokenFeatures(sentence: Sentence) = {
//    /* Elaborate process of computing features */
//    sentence.tokens.foreach { token =>
//      token.attr += new CitationFeatures(token)
//      val preFeats = token.attr[PreFeatures]
//      if (preFeats != null) token.attr[CitationFeatures] ++= preFeats.features
//      wordToFeatures(token)
//    }
//  }

//  def process(document: Document): Document = {
//    if (document.tokens.size == 0) return document
//    // todo this requires iterating over the tokens twice but may be unavoidable if accuracy is improved?
//    model.computeDocumentFeatures(document, training = false)
//    predict(document)
//  }

//  def serialize(stream: OutputStream) {
//    import cc.factorie.util.CubbieConversions._
//    val is = new DataOutputStream(stream)
//    BinarySerializer.serialize(CitationLabelDomain, is)
//    BinarySerializer.serialize(CitationFeaturesDomain.dimensionDomain, is)
//    BinarySerializer.serialize(model, is)
//    is.close()
//  }
//  def deserialize(stream: InputStream): Unit = deserialize(model, stream)
//
//  def deserialize(theModel: CitationCRFModel, stream: InputStream) {
//    import cc.factorie.util.CubbieConversions._
//    val is = new DataInputStream(stream)
//    BinarySerializer.deserialize(CitationLabelDomain, is)
//    CitationLabelDomain.freeze()
//    println(s"Deserialized CitationLabelDomain: ${CitationLabelDomain.dimensionSize}")
//    BinarySerializer.deserialize(CitationFeaturesDomain.dimensionDomain, is)
//    CitationFeaturesDomain.freeze()
//    println(s"Deserialized CitationFeaturesDomain: ${CitationFeaturesDomain.dimensionSize}")
//
//    BinarySerializer.deserialize(theModel, is)
//    println(s"Deserialized model: ${theModel.sparsity}")
//    is.close()
//  }
}


case class ExperimentId(dataSet: String, featureSet: String)

// TODO: implement DocumentAnnotator to add citations
object TrainCitationModel extends HyperparameterMain {

  private val logger = Logger.getLogger(getClass.getName)

  def trainModel(opts: BibieOptions): Double = {
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val lexiconDir = opts.lexiconUrl.value
    val trainer = new CitationCRFTrainer(new DefaultLexicons(lexiconDir))
    val trainDocs = LoadHier.fromFile(opts.trainFile.value)
    val devDocs = LoadHier.fromFile(opts.devFile.value)
    OverSegmenter.overSegment(trainDocs ++ devDocs, lexiconDir)
    val trainEval = trainer.train(trainDocs, devDocs, params)
    logger.info(s"train eval: $trainEval")
    if (opts.saveModel.value) {
      logger.info(s"serializing model to ${opts.modelFile.value}")
      IOHelper.serializeModel(opts.modelFile.value, trainer.model)
    }
    logger.info("load/test deserialized model...")
    val model = IOHelper.deserializeModel(opts.modelFile.value, opts.lexiconUrl.value)
    val ann = new BibieAnnotator(model, opts.lexiconUrl.value)
    devDocs.foreach(ann.process)
    model.evaluate(devDocs, "DEV")
    trainEval
  }

  def trainModelGrobidFeaturesOnly(opts: BibieOptions): Double = {
    logger.info(" ** trainModelGrobidFeaturesOnly **")
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val lexiconDir = opts.lexiconUrl.value
    val trainer = new CitationCRFTrainer(new DefaultLexicons(lexiconDir))
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value)
    val testDocs = LoadGrobid.fromFilename(opts.testFile.value)
    val trainEval = trainer.trainUsingGrobidFeatures(trainDocs, testDocs, params)
    logger.info(s"train eval: $trainEval")
    if (opts.saveModel.value) {
      logger.info(s"serializing model to ${opts.modelFile.value}")
      IOHelper.serializeModel(opts.modelFile.value, trainer.model)
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
    val ann = new BibieAnnotator(trainer.model, opts.lexiconUrl.value)
    evalDocs.foreach(ann.processGrobid)
    val eval = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)
    eval.printEvaluation(evalDocs, "UMASS")
    eval.segmentationEvaluation(evalDocs, "UMASS (SegmentationEvaluation)")

    trainEval
  }

  def trainModelBothFeatureSets(opts: BibieOptions): Double = {
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val lexiconDir = opts.lexiconUrl.value
    val trainer = new CitationCRFTrainer(new DefaultLexicons(lexiconDir))
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value)
    val testDocs = LoadGrobid.fromFilename(opts.testFile.value)
    OverSegmenter.overSegment(trainDocs ++ testDocs, lexiconDir)
    val trainEval = trainer.trainUsingBothFeatureSets(trainDocs, testDocs, params)
    logger.info(s"train eval: $trainEval")
    if (opts.saveModel.value) {
      logger.info(s"serializing model to ${opts.modelFile.value}")
      IOHelper.serializeModel(opts.modelFile.value, trainer.model)
    }
    trainEval
  }

  def trainModelUmassFeatures(opts: BibieOptions): Double = {
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val lexiconDir = opts.lexiconUrl.value
    val trainer = new CitationCRFTrainer(new DefaultLexicons(lexiconDir))
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value)
    val testDocs = LoadGrobid.fromFilename(opts.testFile.value)
    OverSegmenter.overSegment(trainDocs ++ testDocs, lexiconDir)
    val trainEval = trainer.train(trainDocs, testDocs, params)
    logger.info(s"train eval: $trainEval")
    if (opts.saveModel.value) {
      logger.info(s"serializing model to ${opts.modelFile.value}")
      IOHelper.serializeModel(opts.modelFile.value, trainer.model)
    }
    trainEval
  }

  def evaluateParameters(args: Array[String]): Double = {
    println(args.mkString(", "))
    val opts = new BibieOptions
    opts.parse(args)
    println(opts.unParse.mkString(" "))
    val id = ExperimentId(opts.dataSet.value, opts.featureSet.value)
    id match {
      case ExperimentId(DATA_GROBID, FEATURES_GROBID) => trainModelGrobidFeaturesOnly(opts)
      case ExperimentId(DATA_GROBID, FEATURES_UMASS) => trainModelUmassFeatures(opts)
      case ExperimentId(DATA_GROBID, FEATURES_BOTH) => trainModelBothFeatureSets(opts)
      case ExperimentId(DATA_UMASS, FEATURES_UMASS) => trainModel(opts)
      case _ => throw new Exception(s"bad dataSet or featureSet option: ${opts.dataSet.value}, ${opts.featureSet.value}")
    }
  }

  def run(opts: BibieOptions): Double = {
    println(opts.unParse.mkString(" "))
    val id = ExperimentId(opts.dataSet.value, opts.featureSet.value)
    id match {
      case ExperimentId(DATA_GROBID, FEATURES_GROBID) => trainModelGrobidFeaturesOnly(opts)
      case ExperimentId(DATA_GROBID, FEATURES_UMASS) => trainModelUmassFeatures(opts)
      case ExperimentId(DATA_GROBID, FEATURES_BOTH) => trainModelBothFeatureSets(opts)
      case ExperimentId(DATA_UMASS, FEATURES_UMASS) => trainModel(opts)
      case _ => throw new Exception(s"bad dataSet or featureSet option: ${opts.dataSet.value}, ${opts.featureSet.value}")
    }
  }
}

object TestCitationModel {

  def testModel(opts: BibieOptions): Unit = {
    val model = IOHelper.deserializeModel(opts.modelFile.value, opts.lexiconUrl.value)
    val testDocs = LoadHier.fromFile(opts.testFile.value)
    val ann = new BibieAnnotator(model, opts.lexiconUrl.value)
    testDocs.foreach(ann.process)
    model.evaluate(testDocs, "TESTING")
  }

  def main(args: Array[String]): Unit = {
    println(args.mkString(", "))
    val opts = new BibieOptions
    opts.parse(args)
    testModel(opts)



//    val testData = opts.dataset.value match {
//      case "grobid" => LoadGrobid.fromFilename(opts.testFile.value, withFeatures = opts.useGrobidFeatures.value)
//      case _ => LoadHier.fromFile(opts.testFile.value)
//    }
//
//    val tagger = new CitationCRFTrainer(opts.modelFile.value, new DefaultLexicons(lexiconDir))
//    OverSegmenter.overSegment(testData, lexiconDir)
//    testData.foreach{tagger.process}
//    tagger.evaluator.printEvaluationSingle(testData, "TEST FINAL")
//
//    if (opts.outputTagged.wasInvoked) {
//      val outputFname = opts.outputTagged.value
//      println(s"writing tagged output to $outputFname")
//      val writer = new PrintWriter(outputFname, "utf-8")
//      testData.foreach { doc => {
//        doc.tokens.foreach { token => {
//          val label = token.attr[CitationLabel]
//          writer.println(s"${token.string}\t${label.target.categoryValue}\t${label.categoryValue}")
//        }}
//        writer.println()
//      }}
//      writer.close()
//    }
  }
}

object OptimizeCitationModel {
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
    val qs = new QSubExecutor(10, "edu.umass.cs.iesl.bibie.model.TrainCitationModel")
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



//class TrainCitationModelOptions extends cc.factorie.util.DefaultCmdOptions with cc.factorie.app.nlp.SharedNLPCmdOptions {
//  val trainFile = new CmdOption("train-file", "", "STRING", "UMass formatted training file.")
//  val trainDir = new CmdOption("train-dir", "", "STRING", "path to directory of training files")
//  val devFile = new CmdOption("dev-file", "", "STRING", "Umass formatted dev file")
//  val testFile = new CmdOption("test-file", "", "STRING", "UMass formatted testing file (or blank).")
//  val testDir = new CmdOption("test-dir", "", "STRING", "path to directory of testing files")
//  /* misc infrastructure */
//  val evalFile = new CmdOption("eval", "", "STRING", "filename to which to write evaluation")
//  // TODO rename to e.g. "tmpoutput" or something, name is misleading
//  val outputFile = new CmdOption("output", "", "STRING", "output filename")
//  val outputDir = new CmdOption("output-dir", "", "STRING", "directory to write evaluations to")
//  val outputTagged = new CmdOption("output-tagged", "", "STRING", "tagged file output filename")
//  val writeEvals = new CmdOption("write-evals", false, "BOOLEAN", "write evaluations to separate files?")
//  val modelFile = new CmdOption("model-file", "citationCRF.factorie", "STRING", "File for saving model.")
//  val lexiconUrl = new CmdOption("lexicons", "classpath:lexicons", "STRING", "URL prefix for lexicon files/resources named cities, companies, companysuffix, countries, days, firstname.high, ...")
//  val saveModel = new CmdOption("save-model", true, "BOOLEAN", "Whether to save the model or just train/test.")
//  val rootDir = new CmdOption("root-dir", "", "STRING", "path to root directory of project (used for various required IO operations)")
//  val dataset = new CmdOption("dataset", "umass-citation", "STRING", "umass-citation|grobid")
//  val useGrobidFeatures = new CmdOption("use-grobid-features", false, "BOOLEAN", "use grobid features?")
//  /* hyperparameters */
//  val optimizer = new CmdOption("optimizer", "lbfgs", "STRING", "lbfgs|adagrad")
//  val rate = new CmdOption("adagrad-rate", 0.35548827391837345, "FLOAT", "Adagrad learning rate.")
//  val delta = new CmdOption("adagrad-delta", 1.9033917145173614E-6, "FLOAT", "Adagrad delta (ridge).")
//  val l1 = new CmdOption("l1", 0.1, "FLOAT", "l1 regularizer strength")
//  val l2 = new CmdOption("l2", 0.1, "FLOAT", "l2 regularizer strength")
//  val numIterations = new CmdOption("num-iterations", 5, "INT", "Number of training iterations")
//}
//
//class TestCitationModelOptions extends cc.factorie.util.DefaultCmdOptions with cc.factorie.app.nlp.SharedNLPCmdOptions {
//  val testFile = new CmdOption("test-file", "", "STRING", "UMass formatted testing file.")
//  val modelUrl = new CmdOption("model-url", "file://citationCRF.factorie", "STRING", "URL for loading model.")
//  val lexiconUrl = new CmdOption("lexicons", "classpath:lexicons", "STRING", "URL prefix for lexicon files/prefixes named cities, companies, companysuffix, countries, days, firstname.high, ...")
//}




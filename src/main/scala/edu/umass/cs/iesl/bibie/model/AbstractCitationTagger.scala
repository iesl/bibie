package edu.umass.cs.iesl.bibie.model

/**
 * Created by kate on 10/13/15.
 */

import java.io._

import cc.factorie.optimize._
import cc.factorie.app.nlp._
import cc.factorie.app.chain._
import cc.factorie.util.BinarySerializer
import edu.umass.cs.iesl.bibie.evaluate.SegmentationEvaluation
import edu.umass.cs.iesl.bibie.util.DefaultLexicons
import scala.collection.mutable.HashMap


abstract class AbstractCitationTagger extends DocumentAnnotator {
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


  /* DocumentAnnotator methods */
  def process(document: Document): Document = {
    if (document.tokenCount == 0) return document
    if (!document.tokens.head.attr.contains(classOf[CitationFeatures])) {
      addFeatures(document, training = false)
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
  def tokenAnnotationString(token:Token): String = s"${token.attr[CitationLabel].categoryValue}"
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[CitationLabel])

  def addFeatures(document: Document, training: Boolean): Unit
  def train(trainDocuments: Seq[Document], testDocuments: Seq[Document], params: Hyperparams)(implicit random: scala.util.Random): Double = {
    if (params.trimBelow > 0) {
      CitationFeaturesDomain.dimensionDomain.gatherCounts = true
      trainDocuments.foreach(doc => addFeatures(doc, training = true))
      println(s"feature domain size (before cutoff=${params.trimBelow}): ${CitationFeaturesDomain.dimensionDomain.size}")
      CitationFeaturesDomain.dimensionDomain.trimBelowCount(params.trimBelow)
      CitationFeaturesDomain.freeze()
      println(s"feature domain size (after cutoff=${params.trimBelow}): ${CitationFeaturesDomain.dimensionDomain.size}")
      CitationFeaturesDomain.dimensionDomain.gatherCounts = false
      trainDocuments.foreach(doc => doc.tokens.foreach(t => t.attr.remove[CitationFeatures]))
      trainDocuments.foreach(doc => addFeatures(doc, training = true))
    } else {
      trainDocuments.foreach(doc => addFeatures(doc, training = true))
      CitationFeaturesDomain.freeze()
      println(s"feature domain size: ${CitationFeaturesDomain.dimensionDomain.size}")
    }

    testDocuments.foreach(doc => addFeatures(doc, training = false))
    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels: Seq[CitationLabel] = scala.util.Random.shuffle(trainDocuments.flatMap(_.tokens).map(_.attr[CitationLabel]))
    val testLabels: Seq[CitationLabel] = testDocuments.flatMap(_.tokens).map(_.attr[CitationLabel])
    (trainLabels ++ testLabels).filter(_ != null).foreach(_.setRandomly(random))
    val vars = for (td <- trainDocuments; sentence <- td.sentences if sentence.length > 1) yield sentence.tokens.map(_.attr[CitationLabel])
    val examples = vars.map(v => new LikelihoodExample(v.toSeq, model, cc.factorie.infer.InferByBPChain))
    val evaluator = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)
    var iterCount = 0
    def evaluate(): Unit = {
      iterCount += 1
      trainDocuments.foreach(process)
      testDocuments.foreach(process)
      evaluator.printEvaluation(trainDocuments, extra = s"TRAIN($iterCount)")
      evaluator.segmentationEvaluation(trainDocuments, extra = s"TRAIN($iterCount)")
      evaluator.printEvaluation(testDocuments, extra = s"DEV($iterCount)")
      evaluator.segmentationEvaluation(testDocuments, extra = s"DEV($iterCount)")
//      println(top10weights(extra = s"$iterCount"))

    }
    params.optimizer match {
      case "lbfgs" =>
        val optimizer = new LBFGS with L2Regularization
        val trainer = new ThreadLocalBatchTrainer(model.parameters, optimizer)
        trainer.trainFromExamples(examples)
      case "adagrad" =>
        val optimizer = new AdaGrad(delta=params.delta, rate=params.rate) with ParameterAveraging
//        val optimizer = new AdaGradRDA(delta=params.delta, rate=params.rate, l1=params.l1, l2=params.l2, numExamples=examples.length)
        Trainer.onlineTrain(model.parameters, examples, evaluate=evaluate, useParallelTrainer=false, maxIterations=params.numIterations, optimizer=optimizer)
      case _ => throw new Exception(s"invalid optimizer: ${params.optimizer}")
    }
    (trainLabels ++ testLabels).foreach(_.setRandomly(random))
    trainDocuments.foreach(process)
    testDocuments.foreach(process)
    println(top10obsWeights(extra = "final (obs)"))
//    println(top10markovWeights(extra = "final (markov)"))
    evaluator.segmentationEvaluation(trainDocuments, extra = "TRAIN")
    evaluator.printEvaluation(trainDocuments, extra = "TRAIN")
    evaluator.segmentationEvaluation(testDocuments, extra = "DEV")
    evaluator.printEvaluation(testDocuments, extra = "DEV")
  }

  def top10obsWeights(extra: String = ""): String = {
    val sb = new StringBuilder
    sb.append(s"* * * weights ($extra) * * *\n")
    val nfeatures = model.obs.weights.value.dimensions.apply(0)
    val nclasses = model.obs.weights.value.dimensions.apply(1)
    for (i <- 0 to nclasses - 1) {
      val label = CitationLabelDomain.category(i)
      val m = new HashMap[String,Double]()
      for (j <- 0 to CitationFeaturesDomain.dimensionDomain.size - 1) {
        m.put(CitationFeaturesDomain.dimensionDomain.apply(j).toString(), model.obs.weights.value.apply(i, j))
      }
      val srt = m.toList.sortBy { case (featureName, weight) => weight }.reverse
      sb.append(s"* * top weights for class $label * *\n")
      for (j <- 0 to math.min(srt.length, 10)) {
        sb.append(s"\t${srt(j)._2}\t${srt(j)._1}\n")
      }
      sb.append("\n")
    }
    sb.toString()
  }

//  def top10markovWeights(extra: String = ""): String = {
//    val sb = new StringBuilder
//    sb.append(s"* * * weights ($extra) * * *\n")
//    val nfeatures = model.markov.weights.value.dimensions.apply(0)
//    val nclasses = model.markov.weights.value.dimensions.apply(1)
//    for (i <- 0 to nclasses - 1) {
//      val label = CitationLabelDomain.category(i)
//      val m = new HashMap[String,Double]()
//      for (j <- 0 to CitationFeaturesDomain.dimensionDomain.size - 1) {
//        m.put(CitationFeaturesDomain.dimensionDomain.apply(j).toString(), model.markov.weights.value.apply(i, j))
//      }
//      val srt = m.toList.sortBy { case (featureName, weight) => weight }.reverse
//      sb.append(s"* * top weights for class $label * *\n")
//      for (j <- 0 to math.min(srt.length, 10)) {
//        sb.append(s"\t${srt(j)._2}\t${srt(j)._1}\n")
//      }
//      sb.append("\n")
//    }
//    sb.toString()
//  }

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

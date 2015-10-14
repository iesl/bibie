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
    trainDocuments.foreach(doc => addFeatures(doc, training = true))
    println(s"feature domain size: ${CitationFeaturesDomain.dimensionDomain.size}")
    CitationFeaturesDomain.freeze()
    testDocuments.foreach(doc => addFeatures(doc, training = false))
    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels: Seq[CitationLabel] = trainDocuments.flatMap(_.tokens).map(_.attr[CitationLabel])
    val testLabels: Seq[CitationLabel] = testDocuments.flatMap(_.tokens).map(_.attr[CitationLabel])
    (trainLabels ++ testLabels).filter(_ != null).foreach(_.setRandomly(random))
    val vars = for (td <- trainDocuments; sentence <- td.sentences if sentence.length > 1) yield sentence.tokens.map(_.attr[CitationLabel])
    val examples = vars.map(v => new LikelihoodExample(v.toSeq, model, cc.factorie.infer.InferByBPChain))
    val evaluator = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)
    def evaluate(): Unit = {
      trainDocuments.foreach(process)
      testDocuments.foreach(process)
      evaluator.printEvaluation(trainDocuments, extra = "TRAIN")
      evaluator.segmentationEvaluation(trainDocuments, extra = "TRAIN")
      evaluator.printEvaluation(testDocuments, extra = "DEV")
      evaluator.segmentationEvaluation(testDocuments, extra = "DEV")
    }
    params.optimizer match {
      case "lbfgs" =>
        val optimizer = new LBFGS with L2Regularization
        val trainer = new ThreadLocalBatchTrainer(model.parameters, optimizer)
        trainer.trainFromExamples(examples)
      case "adagrad" =>
        val optimizer = new AdaGradRDA(delta=params.delta, rate=params.rate, l1=params.l1, l2=params.l2, numExamples=examples.length)
        Trainer.onlineTrain(model.parameters, examples, evaluate=evaluate, useParallelTrainer=false, maxIterations=params.numIterations, optimizer=optimizer)
      case _ => throw new Exception(s"invalid optimizer: ${params.optimizer}")
    }
    (trainLabels ++ testLabels).foreach(_.setRandomly(random))
    trainDocuments.foreach(process)
    testDocuments.foreach(process)
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

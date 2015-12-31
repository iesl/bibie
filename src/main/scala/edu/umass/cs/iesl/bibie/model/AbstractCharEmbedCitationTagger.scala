package edu.umass.cs.iesl.bibie.model

import java.io._

import cc.factorie.app.chain.ChainModel
import cc.factorie.model.{DotFamilyWithStatistics2, Parameters}
import cc.factorie.optimize._
import cc.factorie.util.BinarySerializer
import cc.factorie.variable.DiscreteDomain
import cc.factorie.variable.Var
import cc.factorie.variable._
import cc.factorie.la.{WeightsMapAccumulator, DenseTensor2, DenseTensor1, Tensor1}
import cc.factorie.app.nlp.{Token, Document}
import cc.factorie._
import edu.umass.cs.iesl.bibie.evaluate.SegmentationEvaluation

import scala.collection.mutable.ListBuffer


/**
 * Created by kate on 12/28/15.
 */
abstract class AbstractCharEmbedCitationTagger(embeddingMap: CharEmbedding,
                              embeddingDim: Int,
                              scale: Double,
                               useOffsetEmbedding: Boolean) {

  object EmbedDomain extends DiscreteDomain(embeddingDim)
  class EmbedVar(t: Tensor1) extends VectorVariable(t) { def domain = EmbedDomain }

  class CitationTaggerCRFModel extends ChainModel[CitationLabel, CitationFeatures, Token](
    CitationLabelDomain,
    CitationFeaturesDomain,
    l => l.token.attr[CitationFeatures],
    l => l.token,
    t => t.attr[CitationLabel]
  ) with Parameters {

    val embedding = new DotFamilyWithStatistics2[CitationLabel, EmbedVar] {
      val weights = Weights(new DenseTensor2(CitationLabelDomain.size, embeddingDim))
    }

    val embeddingPrev = new DotFamilyWithStatistics2[CitationLabel, EmbedVar] {
      val weights = Weights(new DenseTensor2(CitationLabelDomain.size, embeddingDim))
    }

    val embeddingNext = new DotFamilyWithStatistics2[CitationLabel, EmbedVar] {
      val weights = Weights(new DenseTensor2(CitationLabelDomain.size, embeddingDim))
    }

    def mkWordEmbedding(string: String): EmbedVar = {
      val agg = lookup(string)
      new EmbedVar(agg * scale)
    }

    def lookup(string: String): DenseTensor1 = {
      val agg = new DenseTensor1(embeddingDim)
      string.toSeq.flatMap(c => embeddingMap.lookup(c)).map(e => new DenseTensor1(e)).foreach(e => agg += e)
      agg
    }

    override def factors(variables: Iterable[Var]): Iterable[Factor] = {
      val result = new ListBuffer[Factor]
      variables match {
        case labels: Iterable[CitationLabel] =>
          var prevLabel: CitationLabel = null.asInstanceOf[CitationLabel]
          for (label <- labels) {
            result += bias.Factor(label)
            result += obs.Factor(labelToFeatures(label), label)
            if (prevLabel ne null) {
              result += markov.Factor(prevLabel, label)
              if (useObsMarkov) result += obsmarkov.Factor(prevLabel, label, labelToFeatures(label))
            }
            result += embedding.Factor(label, mkWordEmbedding(labelToToken(label).string))
            if (useOffsetEmbedding && labelToToken(label).sentenceHasPrev) {
              result += embeddingPrev.Factor(label, mkWordEmbedding(labelToToken(label).prev.string))
            }
            if (useOffsetEmbedding && labelToToken(label).sentenceHasNext) {
              result += embeddingNext.Factor(label, mkWordEmbedding(labelToToken(label).next.string))
            }
            prevLabel = label
          }
      }
      result
    }

    override def getLocalScores(varying: Seq[CitationLabel]): Array[DenseTensor1] = {
      val biasScores = bias.weights.value
      val obsWeights = obs.weights.value
      val a = Array.fill[DenseTensor1](varying.size)(null)
      var i = 0
      while (i < varying.length) {
        val scores = obsWeights.leftMultiply(labelToFeatures(varying(i)).value.asInstanceOf[Tensor1]).asInstanceOf[DenseTensor1]
        scores += biasScores
        val str = labelToToken(varying(i)).string
        val emb: DenseTensor1 = lookup(str)
        scores += embedding.weights.value * emb
        if (i >= 1) scores += embeddingPrev.weights.value * lookup(labelToToken(varying(i-1)).string)
        if (i < varying.length - 1) scores += embeddingNext.weights.value * lookup(labelToToken(varying(i+1)).string)
        a(i) = scores
        i += 1
      }
      a
    }

    override def accumulateExtraObsGradients(gradient: WeightsMapAccumulator, obs: Tensor1, position: Int, labels: Seq[CitationLabel]): Unit = {
      val emb = lookup(labelToToken(labels(position)).string)
      gradient.accumulate(embedding.weights, obs outer lookup(labelToToken(labels(position)).string))
      if (position >= 1)
        gradient.accumulate(embeddingPrev.weights, obs outer lookup(labelToToken(labels(position - 1)).string))
      if (position < labels.length - 1)
        gradient.accumulate(embeddingNext.weights, obs outer lookup(labelToToken(labels(position + 1)).string))
    }
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
//    val examples = vars.map(v => new LikelihoodExample(v.toSeq, model, cc.factorie.infer.InferByBPChain))
    val examples = vars.map(v => new model.ChainLikelihoodExample(v.toSeq))
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
//        val optimizer = new AdaGradRDA(delta=params.delta, rate=params.rate, l1=params.l1, l2=params.l2, numExamples=examples.length)
        val optimizer = new AdaGrad(delta=params.delta, rate=params.rate) with ParameterAveraging
        Trainer.onlineTrain(model.parameters,
          examples,
          evaluate=evaluate,
          useParallelTrainer=false,
          maxIterations=params.numIterations,
          optimizer=optimizer)
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
//    println(s"model sparsity: ${model.sparsity}")
    is.close()
  }
}

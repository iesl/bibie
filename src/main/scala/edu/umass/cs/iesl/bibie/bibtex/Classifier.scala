package edu.umass.cs.iesl.bibie.bibtex

/**
 * Created by kate on 12/31/15.
 */

import java.util.logging.Logger
import edu.umass.cs.iesl.bibie._
import cc.factorie.app.nlp._
import cc.factorie.optimize._
import cc.factorie.variable._
import cc.factorie.app.strings._
import cc.factorie.app.classify.backend._
import edu.umass.cs.iesl.bibie.model.Hyperparams
import edu.umass.cs.iesl.bibie.util.DefaultLexicons

import scala.collection.mutable.HashMap

/**
 * Classifier for Bibtex data
 */
class Classifier(lexiconPath: String) extends DocumentAnnotator {
  private val logger = Logger.getLogger(getClass.getName)
  val lexicons: DefaultLexicons = new DefaultLexicons(lexiconPath)
  def process(doc: Document): Document = {
    doc
  }
  override def prereqAttrs: Iterable[Class[_]] = Seq(classOf[Token])
  override def postAttrs: Iterable[Class[_]] = Seq(classOf[BibtexLabel])
  override def tokenAnnotationString(token: Token): String = s"${token.attr[BibtexLabel].categoryValue}"

  def train(trainDocuments: Seq[Document], testDocuments: Seq[Document], params: Hyperparams)(implicit random: scala.util.Random): Unit = {
    val trainLabels: Seq[BibtexLabel] = trainDocuments.map(_.attr[BibtexLabel])
    val testLabels: Seq[BibtexLabel] = testDocuments.map(_.attr[BibtexLabel])
    val trainFeatures = trainDocuments.map { doc => addFeatures(doc); doc.attr[BibtexFeatureVar] }
    FeatureDomain.freeze()
    testDocuments.foreach { doc => addFeatures(doc) }
    (trainLabels ++ testLabels).foreach(_.setRandomly(random))
    val trainer = new OnlineLinearMulticlassTrainer()
    val classifier = trainer.train(trainLabels, trainFeatures)
    (trainDocuments ++ testDocuments).foreach { doc =>
      val label = doc.attr[BibtexLabel]
      val features = doc.attr[BibtexFeatureVar]
      label := classifier.classification(features.value).bestLabelIndex
    }
    println(top10weights(classifier, extra = "final"))
    println("")
    val trainEval = new LabeledDiscreteEvaluation[BibtexLabel](trainLabels)
    println("train evaluation")
    println(trainEval)
    println("")
    val eval = new LabeledDiscreteEvaluation[BibtexLabel](testLabels)
    println("test evaluation")
    println(eval)
  }

  def top10weights(model: LinearMulticlassClassifier, extra: String = ""): String = {
    val sb = new StringBuilder
    sb.append(s"* * * weights ($extra) * * *\n")
    val nfeatures = model.weights.value.dimensions.apply(0)
    val nclasses = model.weights.value.dimensions.apply(1)
    for (i <- 0 to nclasses - 1) {
      val label = BibtexDomain.category(i)
      val m = new HashMap[String,Double]()
      for (j <- 0 to FeatureDomain.dimensionDomain.size - 1) {
        m.put(FeatureDomain.dimensionDomain.apply(j).toString(), model.weights.value.apply(i, j))
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

  object FeatureDomain extends CategoricalVectorDomain[String]
  class BibtexFeatureVar(doc: Document) extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain
    val label = doc.attr[BibtexLabel]
  }

  def addFeatures(doc: Document): BibtexFeatureVar = {
    def normalize(t: Token): String = {
      val string = t.string
      simplifyDigits(string.toLowerCase)
    }
    val features = new BibtexFeatureVar(doc)
    doc.tokens.foreach { token =>
      features += normalize(token)
      for (lexicon <- lexicons(token)) {
        features += "LEX=" + lexicon
      }
    }
    doc.attr += features
  }

}

object BibtexDomain extends CategoricalDomain[String]
class BibtexLabel(labelString: String, val doc: Document) extends LabeledCategoricalVariable(labelString) {
  def domain = BibtexDomain
}

object Trainer {
  def main(args: Array[String]): Unit = {
    val opts = new BibieOptions
    opts.parse(args)
    println(opts.unParse.mkString("\n"))
    val lexdir = opts.lexiconUrl.value
    val classifier = new Classifier(lexdir)


  }
}
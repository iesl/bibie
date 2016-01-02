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

  def train(trainDocuments: Seq[Document], testDocuments: Seq[Document], params: Hyperparams)(implicit random: scala.util.Random): LinearMulticlassClassifier = {
    val trainSents: Seq[Sentence] = random.shuffle(trainDocuments.flatMap(_.sentences))
    val trainLabels = trainSents.map(_.attr[BibtexLabel])
    val trainFeats = trainSents.map { s => addFeatures(s); s.attr[BibtexFeatureVar]}
    FeatureDomain.freeze()
    val testSents = random.shuffle(testDocuments.flatMap(_.sentences))
    val testLabels = testSents.map(_.attr[BibtexLabel])
    testSents.foreach(addFeatures)
    (trainLabels ++ testLabels).foreach(_.setRandomly(random))
    val trainer = new OnlineLinearMulticlassTrainer()
    val classifier = trainer.train(trainLabels, trainFeats)
    (trainSents ++ testSents).foreach { sent =>
      val label = sent.attr[BibtexLabel]
      val features = sent.attr[BibtexFeatureVar]
      label := classifier.classification(features.value).bestLabelIndex
    }
    println(top10weights(classifier, extra = "final"))
    println("")
    val trainEval = new LabeledDiscreteEvaluation[BibtexLabel](trainLabels)
    println("train evaluation")
    println(trainEval)
    println(trainEval.overallEvalString)
    println(s"f1: ${trainEval.f1}")
    println("")
    val eval = new LabeledDiscreteEvaluation[BibtexLabel](testLabels)
    println("test evaluation")
    println(eval)
    println(eval.overallEvalString)
    println(s"f1: ${eval.f1}")
    classifier
  }

  def evaluate(classifier: LinearMulticlassClassifier, docs: Seq[Document], extra: String = ""): Unit = {
    val sents = docs.flatMap(_.sentences)
    val vars = sents.map(_.attr[BibtexLabel])
    vars.foreach(_.setRandomly)
    sents.foreach(sent => addFeatures(sent))
    sents.foreach { sent =>
      val label = sent.attr[BibtexLabel]
      val features = sent.attr[BibtexFeatureVar]
      label := classifier.classification(features.value).bestLabelIndex
    }
    println(s"$extra evaluation")
    val eval = new LabeledDiscreteEvaluation[BibtexLabel](vars)
    println(eval)
    println(eval.overallEvalString)
    println(s"f1: ${eval.f1}")
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
  class BibtexFeatureVar(sentence: Sentence) extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain
    val label = sentence.attr[BibtexLabel]
  }

  def addFeatures(sentence: Sentence): BibtexFeatureVar = {
    def normalize(t: Token): String = {
      val string = t.string
      simplifyDigits(string.toLowerCase)
    }
    val features = new BibtexFeatureVar(sentence)
    sentence.tokens.foreach { token =>
      features += normalize(token)
      for (lexicon <- lexicons(token)) {
        features += "LEX=" + lexicon
      }
    }
    sentence.attr += features
  }

}

object BibtexDomain extends CategoricalDomain[String]
class BibtexLabel(labelString: String, val sentence: Sentence) extends LabeledCategoricalVariable(labelString) {
  def domain = BibtexDomain
}

object Trainer {
  def main(args: Array[String]): Unit = {
    val opts = new BibieOptions
    opts.parse(args)
    println(opts.unParse.mkString("\n"))
    val devDocs = LoadBibtex.fromDir(opts.devDir.value)
    devDocs.take(5).foreach { doc =>
      val sents = doc.sentences
      sents.foreach { sent =>
        println(sent.attr[BibtexLabel].target.categoryValue)
        println(sent.tokens.map(_.string).mkString(" "))
      }
      println("")
    }
    val trainDocs = LoadBibtex.fromDir(opts.trainDir.value)
    val lexdir = opts.lexiconUrl.value
    val tagger = new Classifier(lexdir)
    val params = new Hyperparams(opts)
    val model = tagger.train(trainDocs, devDocs, params)
    val testDocs = LoadBibtex.fromDir(opts.testDir.value)
    tagger.evaluate(model, testDocs, extra = "test")
  }
}
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

  val Capitalized = "^[A-Z].*"
  val AllCaps = "^[A-Z]*"
  val Numeric = "^[0-9]+$"
  val ParenNumeric = "^\\([0-9]+\\).?$"
  val Punctuation = "[-,\\.;:?!()]+"
  val EndPeriod = ".*\\.$"
  val EndFullColon = ".*\\:$"
  val EndComma = ".*\\,$"
  val HasOpenParen = ".*\\(.*"
  val HasClosedParen = ".*\\).*"
  val HasOpenSquare = ".*\\[.*"
  val HasClosedSquare = ".*\\].*"
  val ContainsDigit = ".*[0-9].*".r
  val AuthorName = "[A-Z]\\w+"
  val AuthorInitial = "[A-Z][A-Z]?.?$"

  def addFeatures(sentence: Sentence): BibtexFeatureVar = {
    def normalize(t: Token): String = {
      val string = t.string
      simplifyDigits(string.toLowerCase)
    }
    def count(string: String): (Int, Int) = {
      var digits = 0
      var alpha = 0
      for (char <- string) {
        if (char.toString.matches("[0-9]")) digits += 1
        else if (char.toString.matches("[a-zA-Z]")) alpha += 1
      }
      (digits, alpha)
    }
    def addFeature(str: String, pos: Int, features: BibtexFeatureVar): Unit = features += s"$str@$pos"
    def addTokenFeatures(token: Token, features: BibtexFeatureVar): Unit = {
      val tokenPos = token.positionInSentence
      val nword = normalize(token)
      addFeature(s"W=$nword", tokenPos, features)
      for (lexicon <- lexicons(token)) {
        addFeature(s"LEX=$lexicon", tokenPos, features)
      }

      val word = token.string
      val email = """([\w\d\-\_]+)(\+\d+)?@([\w\d\-\.]+)"""
      val url = """((http|ftp|https):\/\/)?[\w\-_]+(\.[\w\-_]+)+([\w\-\.,@?^=%&amp;:/~\+#]*[\w\-\@?^=%&amp;/~\+#])?"""
      val lower = word.toLowerCase
      val replace = lower.replaceAll("\\.|,|\\)", "")
      features += "SIMPLE=" + simplifyDigits(word)
      features += "SIMPLE_LOWER=" + simplifyDigits(word).toLowerCase

      if (word.length > 3) addFeature("PRE=" + word.substring(0, 3), tokenPos, features)
      if (word.matches(Capitalized)) addFeature("CAPITALIZED", tokenPos, features)
      if (word.matches(AllCaps)) addFeature("ALLCAPS", tokenPos, features)
      if (word.matches(Numeric)) addFeature("NUMERIC", tokenPos, features)
      if (word.matches(ParenNumeric)) addFeature("PARENNUMERIC", tokenPos, features)
      if (word.matches(Punctuation)) addFeature("PUNCTUATION", tokenPos, features)
      if (ContainsDigit.findFirstMatchIn(word) != None) addFeature("CONTAINSDIGIT", tokenPos, features)
      if (word.contains(".")) addFeature("CONTAINSDOTS", tokenPos, features)
      if (word.contains("-")) addFeature("CONTAINSDASH", tokenPos, features)
      if (word.matches("[0-9]+\\-[0-9]+")) addFeature("POSSIBLEPAGES", tokenPos, features)
      if (word.matches("[A-Z]")) addFeature("CAPLETTER", tokenPos, features)
      if (word.matches("[a-zA-Z]")) addFeature("SINGLECHAR", tokenPos, features)
      if (word.matches("[A-Z]\\.")) addFeature("LONLEYINITIAL", tokenPos, features)
      if (word.matches(email)) addFeature("EMAIL", tokenPos, features)
      if (word.matches(url)) addFeature("URL", tokenPos, features)
      if (word.matches(EndFullColon)) addFeature("ENDFULLCOLON", tokenPos, features)
      if (word.matches(HasOpenParen)) addFeature("OPENPAREN", tokenPos, features)
      if (word.matches(HasClosedParen)) addFeature("CLOSEDPAREN", tokenPos, features)
      if (word.matches(HasOpenParen) && word.matches(HasClosedParen)) addFeature("OPENANDSHUT", tokenPos, features)
      if (word.matches(HasOpenSquare)) addFeature("OPENSQUARE", tokenPos, features)
      if (word.matches(HasClosedSquare)) addFeature("CLOSEDSQUARE", tokenPos, features)
      if (word.matches(".*[0-9]$")) addFeature("LASTNUM", tokenPos, features)
      if (word.matches(".*[A-Z]$")) addFeature("LASTUPPER", tokenPos, features)
      if (word.matches(".*[a-z]$")) addFeature("LASTLOWER", tokenPos, features)
      if (word.matches(".*\"$")) addFeature("ENDQUOTE", tokenPos, features)
      if (word.matches(".*\".$")) addFeature("QUOTEATEND", tokenPos, features)
      if (word.matches(".*;$")) addFeature("ENDSEMI", tokenPos, features)
      if (word.matches("^\".*")) addFeature("BEGINQUOTE", tokenPos, features)
      if (word.matches(".*\\\\")) addFeature("ENDFORWARD", tokenPos, features)
      if (word.matches("^\\\\.*")) addFeature("BEGINFORWARD", tokenPos, features)
      if (word.matches(".*,\"$")) addFeature("ENDCOMMAQUOTE", tokenPos, features)
      if (word.matches("^[\\'`].*")) addFeature("STARTSINGLEQUOTE", tokenPos, features)
      if (word.matches(".*'.?$")) addFeature("ENDSINGLEQUOTE", tokenPos, features)
      if (word.trim.toLowerCase == "and" || word.trim.toLowerCase == "&") addFeature("ISAND", tokenPos, features)
      if (word.matches(".*[1-9](th|nd|st).*")) addFeature("EVENTITERATION", tokenPos, features)
      if (word.trim.toLowerCase == "thesis" || word.trim.toLowerCase == "dissertation") addFeature("ISTHESIS", tokenPos, features)
      val counts = count(word)
      addFeature("NUMDIGITS=" + counts._1, tokenPos, features)
      addFeature("NUMALPHA=" + counts._2, tokenPos, features)
      addFeature("NUMDIGITS=" + counts._1 + "ALPHS=" + counts._2, tokenPos, features)
      if (replace.matches(Numeric) && counts._1 == 4 && replace.toInt < 1900) addFeature("BEFORE1900", tokenPos, features)
      if (replace.matches(Numeric) && counts._1 == 4 && replace.toInt >= 1900) addFeature("AFTER1900", tokenPos, features)
      //if (word.matches(ParenNumeric) && counts._1 == 4 && word.replaceFirst("\\).?$",")").replaceAll("[\\)\\(]","").toInt < 1900) addFeature("BEFORE1900"
      //if (word.matches(ParenNumeric) && counts._1 == 4 && word.replaceFirst("\\).?$",")").replaceAll("[\\)\\(]","").toInt >= 1900) addFeature("AFTER1900"
      if (lower.startsWith("appeared") || lower.startsWith("submitted") || lower.startsWith("appear")) addFeature("STATUS", tokenPos, features)
      //    if (token.startsSpansOfClass[SegmentSpan].nonEmpty) addFeature("PROBABLESEGMENT"

//      if (docSpans.exists(span => span.tokens.head == token)) addFeature("PROBABLESEGMENT"
      if (lower.matches("(ed\\.|eds\\.|editor|editors).*")) addFeature("EDITOR", tokenPos, features)
      if (lower.matches("(proc\\.?|proceedings|trans\\.?|conf\\.?|symp\\.?|conference|symposium|workshop).*")) addFeature("BOOKTITLE", tokenPos, features)
      if (lower.matches("(university|dept\\.|department).*")) addFeature("INST", tokenPos, features)
      if (lower.matches("^p(p|ages|pps|gs)?\\.?")) addFeature("ISPAGES", tokenPos, features)
      if (lower.matches("(v\\.?|volume|vol\\.?).*")) addFeature("VOLUME", tokenPos, features)
//      loc += 1

      /* added 30 dec 7:55 pm */
//      val pos = token.position.toDouble
//      val len = token.sentence.length.toDouble
//      val relpos = pos / len
//      if (relpos < 0.25) addFeature(s"RELPOS_LT_P25", tokenPos, features)
//      else if (relpos < 0.5) addFeature(s"RELPOS_LT_P50", tokenPos, features)
//      else if (relpos < 0.75) addFeature(s"RELPOS_LT_P75", tokenPos, features)
//      else addFeature(s"RELPOS_LT_P100", tokenPos, features)

      if (word.matches(EndComma)) addFeature("ENDCOMMA", tokenPos, features)
      if (word.matches(EndPeriod)) addFeature("ENDPERIOD", tokenPos, features)
//      if (word.matches(EndComma) || word.matches(EndPeriod) || word.equals(".") || word.equals(",")) {
//        if (token.hasPrev && relpos <= 0.5) {
//          val prev = token.prev
//          val prevStr = prev.string
//          if (prevStr.matches(AuthorInitial)) addFeature("AuthorSegWithInitials", tokenPos, features)
//          if (prevStr.matches(AuthorName)) addFeature("AuthorSeg", tokenPos, features)
//        }
//      }
    }
    val features = new BibtexFeatureVar(sentence)
    sentence.tokens.foreach { token => addTokenFeatures(token, features) }
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
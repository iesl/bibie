package edu.umass.cs.iesl.bibie.model

import cc.factorie.app.nlp.{Token, Document}
import cc.factorie.app.strings._
import cc.factorie.variable.{BinaryFeatureVectorVariable, CategoricalVectorDomain}
import edu.umass.cs.iesl.bibie.load.PreFeatures
import edu.umass.cs.iesl.bibie.segment.CitationSpanList
import edu.umass.cs.iesl.bibie.util.Lexicons

/**
 * @author Kate Silverstein 
 *         created on 9/13/15
 */
object CitationFeaturesDomain extends CategoricalVectorDomain[String]

class CitationFeatures(val token: Token) extends BinaryFeatureVectorVariable[String] {
  def domain = CitationFeaturesDomain
  override def skipNonCategories = true
}

class Features(val lexicons: Lexicons) {

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

  var loc = 0

  // TODO: add some global features to baseline CRF - like "containsthesis"
  def wordToFeatures(token: Token): Unit = {
    val features = new CitationFeatures(token)
    val preFeats = token.attr[PreFeatures]
    if(preFeats != null) features ++= preFeats.features
    val docSpans = token.document.attr[CitationSpanList].spans
    val word = token.string
    val email = """([\w\d\-\_]+)(\+\d+)?@([\w\d\-\.]+)"""
    val url = """((http|ftp|https):\/\/)?[\w\-_]+(\.[\w\-_]+)+([\w\-\.,@?^=%&amp;:/~\+#]*[\w\-\@?^=%&amp;/~\+#])?"""
    val lower = word.toLowerCase
    val replace = lower.replaceAll("\\.|,|\\)", "")
    features += "W=" + word
    features += "SIMPLE=" + simplifyDigits(word)
    features += "SIMPLE_LOWER=" + simplifyDigits(word).toLowerCase
    if (word.length > 3) features += "PRE=" + word.substring(0, 3)
    if (word.matches(Capitalized)) features += "CAPITALIZED"
    if (word.matches(AllCaps)) features += "ALLCAPS"
    if (word.matches(Numeric)) features += "NUMERIC"
    if (word.matches(ParenNumeric)) features += "PARENNUMERIC"
    if (word.matches(Punctuation)) features += "PUNCTUATION"
    if (ContainsDigit.findFirstMatchIn(word) != None) features += "CONTAINSDIGIT"
    if (word.contains(".")) features += "CONTAINSDOTS"
    if (word.contains("-")) features += "CONTAINSDASH"
    if (word.matches("[0-9]+\\-[0-9]+")) features += "POSSIBLEPAGES"
    if (word.matches("[A-Z]")) features += "CAPLETTER"
    if (word.matches("[a-zA-Z]")) features += "SINGLECHAR"
    if (word.matches("[A-Z]\\.")) features += "LONLEYINITIAL"
    if (word.matches(email)) features += "EMAIL"
    if (word.matches(url)) features += "URL"
    if (word.matches(EndComma)) features += "ENDCOMMA"
    if (word.matches(EndPeriod)) features += "ENDPERIOD"
    if (word.matches(EndFullColon)) features += "ENDFULLCOLON"
    if (word.matches(HasOpenParen)) features += "OPENPAREN"
    if (word.matches(HasClosedParen)) features += "CLOSEDPAREN"
    if (word.matches(HasOpenParen) && word.matches(HasClosedParen)) features += "OPENANDSHUT"
    if (word.matches(HasOpenSquare)) features += "OPENSQUARE"
    if (word.matches(HasClosedSquare)) features += "CLOSEDSQUARE"
    if (word.matches(".*[0-9]$")) features += "LASTNUM"
    if (word.matches(".*[A-Z]$")) features += "LASTUPPER"
    if (word.matches(".*[a-z]$")) features += "LASTLOWER"
    if (word.matches(".*\"$")) features += "ENDQUOTE"
    if (word.matches(".*\".$")) features += "QUOTEATEND"
    if (word.matches(".*;$")) features += "ENDSEMI"
    if (word.matches("^\".*")) features += "BEGINQUOTE"
    if (word.matches(".*\\\\")) features += "ENDFORWARD"
    if (word.matches("^\\\\.*")) features += "BEGINFORWARD"
    if (word.matches(".*,\"$")) features += "ENDCOMMAQUOTE"
    if (word.matches("^[\\'`].*")) features += "STARTSINGLEQUOTE"
    if (word.matches(".*'.?$")) features += "ENDSINGLEQUOTE"
    if (word.trim.toLowerCase == "and" || word.trim.toLowerCase == "&") features += "ISAND"
    if (word.matches(".*[1-9](th|nd|st).*")) features += "EVENTITERATION"
    if (word.trim.toLowerCase == "thesis" || word.trim.toLowerCase == "dissertation") features += "ISTHESIS"
    val counts = count(word)
    features += "NUMDIGITS=" + counts._1
    features += "NUMALPHA=" + counts._2
    features += "NUMDIGITS=" + counts._1 + "ALPHS=" + counts._2
    if (replace.matches(Numeric) && counts._1 == 4 && replace.toInt < 1900) features += "BEFORE1900"
    if (replace.matches(Numeric) && counts._1 == 4 && replace.toInt >= 1900) features += "AFTER1900"
    //if (word.matches(ParenNumeric) && counts._1 == 4 && word.replaceFirst("\\).?$",")").replaceAll("[\\)\\(]","").toInt < 1900) features += "BEFORE1900"
    //if (word.matches(ParenNumeric) && counts._1 == 4 && word.replaceFirst("\\).?$",")").replaceAll("[\\)\\(]","").toInt >= 1900) features += "AFTER1900"
    if (lower.startsWith("appeared") || lower.startsWith("submitted") || lower.startsWith("appear")) features += "STATUS"
    //if(token.startsSpansOfClass[SegmentSpan].nonEmpty) features += "PROBABLESEGMENT"
    if (docSpans.exists(span => span.tokens.head == token)) features += "PROBABLESEGMENT"
    if (lower.matches("(ed\\.|eds\\.|editor|editors).*")) features += "EDITOR"
    if (lower.matches("(proc\\.?|proceedings|trans\\.?|conf\\.?|symp\\.?|conference|symposium|workshop).*")) features += "BOOKTITLE"
    if (lower.matches("(university|dept\\.|department).*")) features += "INST"
    if (lower.matches("^p(p|ages|pps|gs)?\\.?")) features += "ISPAGES"
    if (lower.matches("(v\\.?|volume|vol\\.?).*")) features += "VOLUME"
    loc += 1
    token.attr += features
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

  def initSentenceFeatures(d: Document): Unit = {
    val docLength = d.tokens.size
    for (token <- d.tokens) {
      val per = token.position.toDouble / docLength.toDouble
      val binned = ((per * 60.0) / 5.0).floor
      token.attr[CitationFeatures] += "BIN=" + binned
    }

    for (t <- d.tokens) {
      if (t.nextWindow(10).count(_.string.toLowerCase.matches(".*(ed\\.|editor|eds|editors).*")) > 0) t.attr[CitationFeatures] += "POSSIBLEEDITOR"
    }

    for (s <- d.sentences) {
      for (token <- s.tokens) {
        for (lexicon <- lexicons(token)) {
          token.attr[CitationFeatures] += "LEX=" + lexicon
        }
      }
      cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(s.tokens, (t: Token) => t.attr[CitationFeatures], "^[^@]*$", List(0, 0), List(1), List(2), List(-1), List(-2))
    }
  }

  def initSentenceFeatures(data: Seq[Document]): Unit = {
    for (d <- data) {
      val docLength = d.tokens.size
      for (token <- d.tokens) {
        val per = token.position.toDouble / docLength.toDouble
        val binned = ((per * 60.0) / 5.0).floor
        token.attr[CitationFeatures] += "BIN=" + binned
      }
    }

    for (d <- data) {
      for (t <- d.tokens) {
        if (t.nextWindow(10).count(_.string.toLowerCase.matches(".*(ed\\.|editor|eds|editors).*")) > 0) t.attr[CitationFeatures] += "POSSIBLEEDITOR"
      }
    }

    for (d <- data) {
      for (s <- d.sentences) {
        for (token <- s.tokens) {
          for (lexicon <- lexicons(token)) {
            token.attr[CitationFeatures] += "LEX=" + lexicon
          }
        }
        cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(s.tokens, (t: Token) => t.attr[CitationFeatures], "^[^@]*$", List(0, 0), List(1), List(2), List(-1), List(-2))
      }
    }
  }

}

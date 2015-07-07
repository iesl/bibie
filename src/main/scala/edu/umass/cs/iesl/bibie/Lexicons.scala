package edu.umass.cs.iesl.bibie

import cc.factorie.app.nlp.{Document, Sentence, Token, TokenSpan}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class DefaultLexicons(urlPrefix: String) extends Lexicons(urlPrefix, List("institution.lst", "tech.lst", "note.lst", "WikiLocations.lst", "WikiLocationsRedirects.lst", "WikiOrganizations.lst", "WikiOrganizationsRedirects.lst",
"cardinalNumber.txt", "known_corporations.lst", "known_country.lst", "known_name.lst", "known_names.big.lst", "known_state.lst", "temporal_words.txt", "authors.lst",
"journal.lst", "names.lst", "publishers.lst"))

class Lexicons(val urlPrefix: String, val lexicons: List[String]) {

  val lexiconMap = mutable.HashMap[String, List[String]]()
  val lexiconNames = ArrayBuffer[String]()

  for (filename <- lexicons) {
    lexiconNames += filename
    println("Reading lexicon " + filename)
    val source = Source.fromURL(urlPrefix + "/" + filename)
    for (l <- source.getLines())
      if (removeTrail(l).length > 2)
        setOrUpdate(removeTrail(l), filename)
    source.close()
  }

  def removeTrail(s: String): String = s.replaceAll("\\.|,|\\(|\\)", "").replaceAll("  +", " ").trim.toLowerCase

  def setOrUpdate(s: String, filename: String): Unit =
    if (lexiconMap.contains(s.toLowerCase))
      lexiconMap(s) = lexiconMap(s) ++ List(filename)
    else
      lexiconMap(s) = List(filename)

  def apply(token: Token): List[String] = {
    val phrase = getPhrase(token)
    val keys = subsect(phrase, token, 7).filter(k => removeTrail(k.head.string) != "" && removeTrail(k.last.string) != "")

    var lexes = List[String]()
    for (keyPre <- keys) {
      val key = removeTrail(keyPre.map(_.string).mkString(" "))
      if (lexiconMap.contains(key) && (removeTrail(token.string) != "" || (keyPre.head.position < token.position && keyPre.last.position > token.position))) {
        lexes = lexiconMap(key).map(locate(token, keyPre) +) ::: lexes
      }
    }
    lexes
  }

  def apply(span: TokenSpan): List[String] =
    if (lexiconMap.contains(removeTrail(span.string))) lexiconMap(removeTrail(span.string)) else List[String]()

  def apply(span: String): List[String] =
    if (lexiconMap.contains(span)) lexiconMap(span) else List[String]()

  def subsect(phrase: Seq[Token], token: Token, maxOutLength: Int): List[List[Token]] = {
    val middle = phrase.zipWithIndex.filter(_._1 == token).head._2
    var keys = List[List[Token]]()
    for (i <- 0 to maxOutLength) {
      var start = middle
      for (j <- 0 to i) {
        start = middle - j
        var key = List[Token]()
        if (start > -1 && (start + i) < phrase.size) {
          for (k <- 0 to i) {
            key = key ++ List(phrase(start + k))
          }
          keys = key :: keys
        }
      }
    }
    keys
  }

  def locate(token: Token, key: List[Token]): String =
    if (key.length == 1) "U-"
    else if (token.position == key.head.position) "B-"
    else if (token.position == key.last.position) "L-"
    else "I-"

  def getPhrase(token: Token): Seq[Token] = {
    val fullPhrase = new ArrayBuffer[Token]()
    val start = if (token.position - 7 >= 0) token.position - 7 else 0
    val end = if (token.position + 7 < token.document.tokens.size) token.position + 7 else token.document.tokens.size - 1
    for (i <- start to end) fullPhrase += token.document.asSection.tokens(i)
    fullPhrase.toSeq
  }
}

object Lexicons {
  var lo: Lexicons = null
  def main(args: Array[String]) {
    val lexicons = new DefaultLexicons("src/main/resources/lexicons")
    val docs = new ArrayBuffer[Document]()
    for (l <- Source.fromURL(args(0)).getLines()) {
      val d = new Document("a" + docs.length)
      val s = new Sentence(d)
      l.split(" ").foreach {t => if (s.length > 0) d.appendString(" "); new Token(s, t);}
      docs += d
    }
    docs.foreach {
      d =>
        println(d.string)
        d.tokens.foreach(t => println(t.string + " : " + lexicons(t).mkString(" , ")))
    }
  }
}
package edu.umass.cs.iesl.bibie.model

/**
 * Created by kate on 10/13/15.
 */

import java.net.URL
import java.util.logging.Logger

import cc.factorie.app.nlp._
import cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions
import edu.umass.cs.iesl.bibie.util.DefaultLexicons

class DefaultCitationTagger(lexiconPath: String) extends AbstractCitationTagger {

  private val logger = Logger.getLogger(getClass.getName)
  lazy val featureBuilder = new Features(lexicons)

  /* Deserialize this tagger from the model at the given URL */
  def this(lexiconPath: String, url:java.net.URL) = {
    this(lexiconPath)
    if (url != null) {
      deserialize(url.openConnection.getInputStream)
      logger.info(s"loaded model from ${url.getPath}")
    }
    else {
      logger.info(s"model not found at ${url.getPath}")
    }
  }

  /* Deserialize this tagger from the model at the given path on disk */
  def this(lexiconPath: String, modelPath: String) = {
    this(lexiconPath, new URL("file://" + modelPath))
  }

  /*
  List("institution.lst", "tech.lst", "note.lst", "WikiLocations.lst", "WikiLocationsRedirects.lst", "WikiOrganizations.lst", "WikiOrganizationsRedirects.lst",
"cardinalNumber.txt", "known_corporations.lst", "known_country.lst", "known_name.lst", "known_names.big.lst", "known_state.lst", "temporal_words.txt", "authors.lst",
"journal.lst", "names.lst", "publishers.lst"))
   */
  val lexicons: DefaultLexicons = new DefaultLexicons(lexiconPath)
  val authorLexicons: Set[String] = Set("known_name.lst", "known_names.big.lst", "authors.lst", "names.lst")
  var printCount = 0
  case class LexiconList(hits: List[String]) {
    val baseValues = hits.map(h => h.split("-").last)
  }

  def addFeatures(doc: Document, training: Boolean = false): Unit = {
    def possibleAuthor(token: Token): Boolean = {
      token.attr[LexiconList].baseValues.exists(hit => authorLexicons.contains(hit))
    }
    def addSentenceFeatures(sent: Sentence): Unit = {
      sent.tokens.foreach { token =>
        val features = new CitationFeatures(token)
        val lexiconHits = token.attr[LexiconList].baseValues
        for (lexicon <- lexiconHits) features += s"LEX=$lexicon"
        token.attr += features
        featureBuilder.wordToFeatures(token)
        if (token.nextWindow(10).count(_.string.toLowerCase.matches(".*(ed\\.|editor|eds|editors).*")) > 0)
          features += "POSSIBLEEDITOR"
        if (token.nextWindow(10).count(_.string.toLowerCase.matches("[bB]y")) > 0)
          features += "NearBy"       /* added 3 Jan 1:13 PM */
        if (token.prevWindow(10).count(possibleAuthor) > 0) {
          features += "NearAuthors"
          val tokenIsAuthor = possibleAuthor(token)
          val currStr = token.string
          val tokenIsInitials = currStr.length <= 2 && currStr.forall(c => c.isUpper)
          val nextIsPunct = if (token.hasNext) token.next.isPunctuation else false
          val nextIsPeriod = if (nextIsPunct) token.next.string.equals(".") else false
          if (tokenIsAuthor) features += "PossibleEndAuthors"
          if (tokenIsAuthor && nextIsPunct) features += "AuthorThenPunct"
          if (tokenIsInitials && nextIsPunct) features += "InitialsThenPunct"
          if (tokenIsInitials && nextIsPeriod) features += "InitialsThenPeriod"
        }
      }
      addNeighboringFeatureConjunctions(sent.tokens,
                                        (t: Token) => t.attr[CitationFeatures],
                                        "^[^@]*$",
                                        List(0, 0), List(1), List(2), List(-1), List(-2))
    }
    assert(lexicons != null)
    // build list of lexicon hits beforehand
    doc.tokens.par.foreach { token =>
      token.attr += new LexiconList(lexicons(token))
    }
    if (printCount < 50) {
      doc.tokens.foreach { token =>
        val hits = token.attr[LexiconList].hits
        println(s"${token.string}\t${hits.mkString(", ")}")
      }
      printCount += 1
    }
    if (training) {
      val sentenceIter = doc.sentences.toIterator
      while (sentenceIter.hasNext) {
        val sentence = sentenceIter.next()
        if (sentence.nonEmpty) addSentenceFeatures(sentence)
      }
    } else { // do it in parallel
      doc.sentences.filter(_.nonEmpty).par.foreach { addSentenceFeatures }
    }
  }

}

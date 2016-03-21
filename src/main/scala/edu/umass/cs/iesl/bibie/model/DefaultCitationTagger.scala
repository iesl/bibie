package edu.umass.cs.iesl.bibie.model

/**
 * Created by kate on 10/13/15.
 */

import java.net.URL
import java.util.logging.Logger

import cc.factorie.app.nlp._
import edu.umass.cs.iesl.bibie.util.DefaultLexicons

class DefaultCitationTagger(logFilename: Option[String], lexiconPath: String) extends AbstractCitationTagger(logFilename) {

  private val logger = Logger.getLogger(getClass.getName)

  /* Deserialize this tagger from the model at the given URL */
  def this(logFilename: Option[String], lexiconPath: String, url:java.net.URL) = {
    this(logFilename, lexiconPath)
    if (url != null) {
      deserialize(url.openConnection.getInputStream)
      logger.info(s"loaded model from ${url.getPath}")
    }
    else {
      logger.info(s"model not found at ${url.getPath}")
    }
  }

  /* Deserialize this tagger from the model at the given path on disk */
  def this(logFilename: Option[String], lexiconPath: String, modelPath: String) = {
    this(logFilename, lexiconPath, new URL("file://" + modelPath))
  }

  val lexicons: DefaultLexicons = new DefaultLexicons(lexiconPath)

  def addFeatures(doc: Document): Unit = {
    assert(lexicons != null)
    val featureBuilder = new Features(lexicons)
    val sentenceIter = doc.sentences.toIterator
    while (sentenceIter.hasNext) {
      val sentence = sentenceIter.next()
      if (sentence.nonEmpty) {
        sentence.tokens.foreach { token =>
          if (token.attr[CitationFeatures] == null) token.attr += new CitationFeatures(token)
          featureBuilder.wordToFeatures(token)
        }
      }
    }
    featureBuilder.initSentenceFeatures(doc)
  }

}

package edu.umass.cs.iesl.bibie.model

/**
 * Created by kate on 10/13/15.
 */

import cc.factorie.app.nlp._
import edu.umass.cs.iesl.bibie.load.PreFeatures

import java.net.URL
import java.util.logging.Logger

class GrobidCitationTagger(logFilename: Option[String]) extends AbstractCitationTagger(logFilename) {

  private val logger = Logger.getLogger(getClass.getName)

  /* Deserialize this tagger from the model at the given URL */
  def this(logFilename: Option[String], url:java.net.URL) = {
    this(logFilename)
    if (url != null) {
      deserialize(url.openConnection.getInputStream)
      logger.info(s"loaded model from ${url.getPath}")
    } else {
      logger.info(s"model not found at ${url.getPath}")
    }
  }

  /* Deserialize this tagger from the model at the given path on disk */
  def this(logFilename: Option[String], modelPath: String) = {
    this(logFilename, new URL("file://" + modelPath))
  }

  def addFeatures(doc: Document): Unit = {
    doc.tokens.foreach { token =>
      val features = new CitationFeatures(token)
      features ++= token.attr[PreFeatures].features
      token.attr += features
    }
  }

}

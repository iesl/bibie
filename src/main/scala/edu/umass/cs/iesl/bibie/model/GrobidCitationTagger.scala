package edu.umass.cs.iesl.bibie.model

/**
 * Created by kate on 10/13/15.
 */

import java.io.File
import java.net.URL

import cc.factorie.app.nlp._
import edu.umass.cs.iesl.bibie.load.PreFeatures

class GrobidCitationTagger(logFilename: Option[String]) extends AbstractCitationTagger(logFilename) {

  def this(logFilename: Option[String], url: URL) = {
    this(logFilename)
    deserialize(url.openConnection().getInputStream)
    log.info(s"deserialized model from ${url.getPath}")
  }

  def this(logFilename: Option[String], path: String) = this(logFilename, new File(path).toURL)

  def addFeatures(doc: Document): Unit = {
    doc.tokens.foreach { token =>
      val features = new CitationFeatures(token)
      features ++= token.attr[PreFeatures].features
      token.attr += features
    }
  }

}

package edu.umass.cs.iesl.bibie.model

/**
 * Created by kate on 10/13/15.
 */

import java.io.File
import java.net.URL

import cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions
import cc.factorie.app.nlp._
import edu.umass.cs.iesl.bibie.util.DefaultLexicons

class DefaultCitationTagger(logFilename: Option[String], lexiconPath: String) extends AbstractCitationTagger(logFilename) {

  def this(logFilename: Option[String], lexiconPath: String, url: URL) = {
    this(logFilename, lexiconPath)
    deserialize(url.openConnection().getInputStream)
    log.info(s"deserialized model from ${url.getPath}")
  }

  def this(logFilename: Option[String], lexiconPath: String, path: String) = this(logFilename, lexiconPath, new File(path).toURL)

  val lexicons: DefaultLexicons = new DefaultLexicons(lexiconPath)

  def addFeatures(doc: Document): Unit = {
    val vf = (t: Token) => t.attr[CitationFeatures]
    val tokenSequence = doc.tokens.toSeq
    tokenSequence.foreach { t =>
      t.attr += new CitationFeatures(t)
      vf(t) ++= TokenFeatures(t)
    }
    addNeighboringFeatureConjunctions(doc.tokens.toIndexedSeq, vf, "^[^@]*$", List(0), List(1), List(2), List(-1), List(-2))
  }

}

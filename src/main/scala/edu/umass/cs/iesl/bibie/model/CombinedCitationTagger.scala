package edu.umass.cs.iesl.bibie.model

import java.net.URL
import java.util.logging.Logger

import cc.factorie.app.chain.Observations._
import cc.factorie.app.nlp.{Document, Token}
import edu.umass.cs.iesl.bibie.load.PreFeatures
import edu.umass.cs.iesl.bibie.util.DefaultLexicons

/**
 * Created by kate on 10/13/15.
 */
class CombinedCitationTagger(logFilename: Option[String], lexiconPath: String) extends AbstractCitationTagger(logFilename) {

  private val logger = Logger.getLogger(getClass.getName)

  /* Deserialize this tagger from the model at the given URL */
  def this(logFilename: Option[String], lexiconPath: String, url:java.net.URL) = {
    this(logFilename, lexiconPath)
    if (url != null) {
      deserialize(url.openConnection.getInputStream)
      logger.info(s"loaded model from ${url.getPath}")
    } else {
      logger.info(s"model not found at ${url.getPath}")
    }
  }

  /* Deserialize this tagger from the model at the given path on disk */
  def this(logFilename: Option[String], lexiconPath: String, modelPath: String) = {
    this(logFilename, lexiconPath, new URL("file://" + modelPath))
  }

  val lexicons: DefaultLexicons = new DefaultLexicons(lexiconPath)

  def addFeatures(doc: Document): Unit = {
    val vf = (t: Token) => t.attr[CitationFeatures]
    val tokenSequence = doc.tokens.toSeq
    tokenSequence.foreach { token =>
      token.attr += new CitationFeatures(token)
      assert(token.attr[PreFeatures] ne null)
      token.attr[CitationFeatures] ++= token.attr[PreFeatures].features
      vf(token) ++= TokenFeatures(token)
    }
    addNeighboringFeatureConjunctions(doc.tokens.toIndexedSeq, vf, "^[^@]*$", List(0), List(1), List(2), List(-1), List(-2))
  }

}

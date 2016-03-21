package edu.umass.cs.iesl.bibie.model

import cc.factorie.app.nlp.{Sentence, Document}
import edu.umass.cs.iesl.bibie.load.PreFeatures
import edu.umass.cs.iesl.bibie.util.DefaultLexicons

import java.net.URL
import java.util.logging.Logger

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
    computeDocumentFeaturesGrobid(doc)
    addDefaultFeatures(doc)
  }

  def computeDocumentFeaturesGrobid(doc: Document): Unit = {
    val sentenceIter = doc.sentences.toIterator
    while (sentenceIter.hasNext) {
      val sentence = sentenceIter.next()
      if (sentence.nonEmpty) {
        computeTokenFeaturesGrobid(sentence)
      }
    }
  }

  def computeTokenFeaturesGrobid(sentence: Sentence): Unit = {
    sentence.tokens.foreach { token =>
      if (token.attr[CitationFeatures] == null) {
        token.attr += new CitationFeatures(token)
      }
      if (token.attr[PreFeatures] != null) {
        token.attr[CitationFeatures] ++= token.attr[PreFeatures].features
      }
    }
  }

  def addDefaultFeatures(doc: Document, training: Boolean = false): Unit = {
    assert(lexicons != null)
    val featureBuilder = new Features(lexicons)
    if (training) {
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
    } else {
      doc.sentences.filter(_.nonEmpty).par.foreach { sentence =>
        sentence.tokens.foreach { token =>
          if (token.attr[CitationFeatures] == null) token.attr += new CitationFeatures(token)
          featureBuilder.wordToFeatures(token)
        }
      }
    }
    featureBuilder.initSentenceFeatures(doc)
  }

}

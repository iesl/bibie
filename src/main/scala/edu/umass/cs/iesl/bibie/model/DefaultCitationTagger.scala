package edu.umass.cs.iesl.bibie.model

/**
 * Created by kate on 10/13/15.
 */

import java.net.URL

import cc.factorie.app.nlp._
import edu.umass.cs.iesl.bibie.util.DefaultLexicons

class DefaultCitationTagger(lexiconPath: String) extends AbstractCitationTagger {

  /* Deserialize this tagger from the model at the given URL */
  def this(lexiconPath: String, url:java.net.URL) = {
    this(lexiconPath)
    if (url != null) {
      deserialize(url.openConnection.getInputStream)
      println("Found model")
    }
    else {
      println("model not found")
    }
  }

  /* Deserialize this tagger from the model at the given path on disk */
  def this(lexiconPath: String, modelPath: String) = {
    this(lexiconPath, new URL("file://" + modelPath))
  }

  val lexicons: DefaultLexicons = new DefaultLexicons(lexiconPath)

  def addFeatures(doc: Document, training: Boolean = false): Unit = {
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

  override def train(trainDocuments: Seq[Document], testDocuments: Seq[Document], params: Hyperparams)(implicit random: scala.util.Random): Double = {
    lexicons = new DefaultLexicons(params.lexiconUrl)
    super.train(trainDocuments, testDocuments, params)
  }

}

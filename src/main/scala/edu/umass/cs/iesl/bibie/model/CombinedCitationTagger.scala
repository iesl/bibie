package edu.umass.cs.iesl.bibie.model

import java.net.URL

import cc.factorie.app.nlp.{Sentence, Document}
import edu.umass.cs.iesl.bibie.load.PreFeatures
import edu.umass.cs.iesl.bibie.util.DefaultLexicons

/**
 * Created by kate on 10/13/15.
 */
class CombinedCitationTagger(lexiconPath: String) extends AbstractCitationTagger {

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
  def this(lexcionPath: String, modelPath: String) = {
    this(lexiconPath, new URL("file://" + modelPath))
  }

  val lexicons: DefaultLexicons = new DefaultLexicons(lexiconPath)

  def addFeatures(doc: Document, training: Boolean = false): Unit = {
    computeDocumentFeaturesGrobid(doc, training = training)
    addFeatures(doc, training = training)
  }

  def computeDocumentFeaturesGrobid(doc: Document, training: Boolean): Unit = {
    if (training) {
      val sentenceIter = doc.sentences.toIterator
      while (sentenceIter.hasNext) {
        val sentence = sentenceIter.next()
        if (sentence.nonEmpty) {
          computeTokenFeaturesGrobid(sentence)
        }
      }
    } else {
      doc.sentences.filter(_.nonEmpty).par.foreach { sentence =>
        computeTokenFeaturesGrobid(sentence)
      }
    }
  }

  def computeTokenFeaturesGrobid(sentence: Sentence): Unit = {
    sentence.tokens.foreach { token =>
      val features = new CitationFeatures(token)
      if (token.attr[PreFeatures] != null) {
        features ++= token.attr[PreFeatures].features
      }
      token.attr += features
    }
  }

  override def train(trainDocuments: Seq[Document], testDocuments: Seq[Document], params: Hyperparams)(implicit random: scala.util.Random): Double = {
    super.train(trainDocuments, testDocuments, params)
  }

}

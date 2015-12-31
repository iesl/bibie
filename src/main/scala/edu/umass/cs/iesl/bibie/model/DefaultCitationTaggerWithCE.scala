package edu.umass.cs.iesl.bibie.model

import cc.factorie.app.nlp.{Sentence, Document}
import edu.umass.cs.iesl.bibie.util.DefaultLexicons

/**
 * Created by kate on 12/30/15.
 */
class DefaultCitationTaggerWithCE(lexiconPath: String,
                                   embeddingMap: CharEmbedding,
                                   embeddingDim: Int,
                                   scale: Double,
                                   useOffsetEmbedding: Boolean) extends AbstractCharEmbedCitationTagger(embeddingMap, embeddingDim, scale, useOffsetEmbedding) {

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
}
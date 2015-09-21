package edu.umass.cs.iesl.bibie

import cc.factorie.app.nlp._

import edu.umass.cs.iesl.bibie.model.{CitationCRFModel, CitationFeatures, CitationLabel, CitationBIOHelper}
import edu.umass.cs.iesl.bibie._

/**
 * @author Kate Silverstein 
 *         created on 9/20/15
 */
class BibieAnnotator(model: CitationCRFModel) extends DocumentAnnotator {

  override def process(document: Document): Document = {
    if (document.tokenCount == 0) return document
    if (document.tokens.head.attr[CitationFeatures] == null) {
      model.computeDocumentFeatures(document, training = false)
    }
    document.tokens.foreach { token =>
      if (token.attr[CitationLabel] == null) {
        token.attr += new CitationLabel("O", token)
      }
    }
    //TODO set labels randomly here?
    val sentenceIter = document.sentences.toIterator
    while (sentenceIter.hasNext) {
      val sentence = sentenceIter.next()
      val vars = sentence.tokens.map(_.attr[CitationLabel]).toSeq
      val sum = CitationBIOHelper.infer(vars, model)
      sum.setToMaximize(null)
    }
    document
  }

  override def postAttrs: Iterable[Class[_]] = Seq(classOf[CitationLabel])

  override def prereqAttrs: Iterable[Class[_]] = Seq(classOf[Sentence], classOf[Token])

  override def tokenAnnotationString(token: Token): String = token.attr[CitationLabel].categoryValue

  def getXML(doc: Document): String = {
    doc.toXML()
  }

}

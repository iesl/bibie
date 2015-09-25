package edu.umass.cs.iesl.bibie

import cc.factorie.app.nlp._

import edu.umass.cs.iesl.bibie.model._
import edu.umass.cs.iesl.bibie.segment._
import edu.umass.cs.iesl.bibie._

import scala.collection.mutable

import java.util.logging.Logger

/**
 * @author Kate Silverstein 
 *         created on 9/20/15
 */
class BibieAnnotator(model: CitationCRFModel, lexiconUrlBase: String) extends DocumentAnnotator {

  private val logger = Logger.getLogger(getClass.getName)

  lazy val segmenter = new OverSegmenterAnnotator(lexiconUrlBase)

  def processDocs(docs: Seq[Document]): Seq[Document] = {
    if (docs.head.attr[CitationSpanList] == null) {
      OverSegmenter.overSegment(docs, lexiconUrlBase)
    }
    docs.map(process)
  }

  override def process(document: Document): Document = {
    if (document.tokenCount == 0) return document
    if (document.attr[CitationSpanList] == null) {
      segmenter.process(document)
    }
    if (document.tokens.head.attr[CitationFeatures] == null) {
      model.computeDocumentFeatures(document, training = false)
    }
    document.tokens.foreach { token =>
      if (token.attr[CitationLabel] == null) {
        token.attr += new CitationLabel("", token)
      }
    }
    //TODO set labels randomly here?
    val sentenceIter = document.sentences.toIterator
    while (sentenceIter.hasNext) {
      val sentence = sentenceIter.next()
      if (sentence.length > 0) {
        val vars = sentence.tokens.map(_.attr[CitationLabel]).toSeq
        val sum = CitationBIOHelper.infer(vars, model)
        sum.setToMaximize(null)
      }
    }
    // remove features
    document.tokens.foreach { token =>
      if (token.attr[CitationFeatures] != null) {
        token.attr.remove[CitationFeatures]
      }
    }
    // add CitationSpanBuffer
    val buff = new CitationSpanBuffer
    buff ++= makeCitationSpans(document)
    document.attr += buff
    document
  }

  def processGrobid(document: Document): Document = {
    if (document.tokenCount == 0) return document
    if (document.tokens.head.attr[CitationFeatures] == null) {
      model.computeDocumentFeaturesGrobid(document, training = false)
    }
    document.tokens.foreach { token =>
      if (token.attr[CitationLabel] == null) {
        token.attr += new CitationLabel("", token)
      }
    }
    //TODO set labels randomly here?
    val sentenceIter = document.sentences.toIterator
    while (sentenceIter.hasNext) {
      val sentence = sentenceIter.next()
      if (sentence.length > 0) {
        val vars = sentence.tokens.map(_.attr[CitationLabel]).toSeq
        val sum = CitationBIOHelper.infer(vars, model)
        sum.setToMaximize(null)
      }
    }
//    // remove features
//    document.tokens.foreach { token =>
//      if (token.attr[CitationFeatures] != null) {
//        token.attr.remove[CitationFeatures]
//      }
//    }
//    // add CitationSpanBuffer
//    val buff = new CitationSpanBuffer
//    buff ++= makeCitationSpans(document)
//    document.attr += buff
    document
  }

  def processBoth(document: Document): Document = {
    if (document.tokenCount == 0) return document
    if (document.attr[CitationSpanList] == null) {
      segmenter.process(document)
    }
    if (document.tokens.head.attr[CitationFeatures] == null) {
      model.computeDocumentFeaturesBoth(document, training = false)
    }
    document.tokens.foreach { token =>
      if (token.attr[CitationLabel] == null) {
        token.attr += new CitationLabel("", token)
      }
    }
    //TODO set labels randomly here?
    val sentenceIter = document.sentences.toIterator
    while (sentenceIter.hasNext) {
      val sentence = sentenceIter.next()
      if (sentence.length > 0) {
        val vars = sentence.tokens.map(_.attr[CitationLabel]).toSeq
        val sum = CitationBIOHelper.infer(vars, model)
        sum.setToMaximize(null)
      }
    }
    //    // remove features
    //    document.tokens.foreach { token =>
    //      if (token.attr[CitationFeatures] != null) {
    //        token.attr.remove[CitationFeatures]
    //      }
    //    }
    //    // add CitationSpanBuffer
    //    val buff = new CitationSpanBuffer
    //    buff ++= makeCitationSpans(document)
    //    document.attr += buff
    document
  }


  /**
   * Same as process(), but don't remove features from tokens. Method to be called during training for evaluation
   * purpose.
   * @param document some FACTORIE document
   * @return document with CitationLabel annotations
   */
  def processDuringTraining(document: Document): Document = {
    if (document.tokenCount == 0) return document
    if (document.attr[CitationSpanList] == null) {
      segmenter.process(document)
    }
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

  override def postAttrs: Iterable[Class[_]] = Seq(classOf[CitationLabel], classOf[CitationSpanBuffer])

  // assumes OverSegmenter has already been run on document
  override def prereqAttrs: Iterable[Class[_]] = Seq(classOf[Sentence], classOf[Token], classOf[CitationSpanList])

  override def tokenAnnotationString(token: Token): String = token.attr[CitationLabel].categoryValue

  def makeCitationSpans(doc: Document): Seq[CitationSpan] = {
    val chunks = new mutable.ArrayBuffer[CitationSpan]()
    var currChunk = new mutable.ArrayBuffer[Token]()
    val tokenSeq = doc.tokens.toSeq
    var i = 0
    while (i < tokenSeq.length) {
      val curr = tokenSeq(i)
      val label = curr.attr[CitationLabel].categoryValue
      val parts = label.split(":")
      var isBegin = false
      var partsIdx = -1
      var j = 0
      while (j < parts.length && !isBegin) {
        val currPart = parts(j)
        if (currPart.startsWith("B-")) {
          isBegin = true
          partsIdx = j
        }
        j += 1
      }
      if (isBegin && currChunk.length > 0) {
        val spanLabel = parts.take(partsIdx).mkString(":")
        val spanStart = currChunk.head.position
        val spanLen = currChunk.length
        val span = new CitationSpan(doc, spanLabel, spanStart, spanLen)
        chunks += span
        currChunk.clear()
        currChunk += curr
      } else {
        currChunk += curr
      }
      i += 1
    }
    chunks.toSeq
  }

}

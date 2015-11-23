package edu.umass.cs.iesl.bibie.evaluate

import cc.factorie.app.nlp.{Token, Document}
import edu.umass.cs.iesl.bibie.model.CitationLabel
import scala.collection.mutable.ArrayBuffer

/**
 * Created by kate on 11/23/15.
 */

case class Label(label: CitationLabel) {
  val groundTruth = label.target.categoryValue
  val truePrefix = groundTruth.split("-").head
  val trueCategory = groundTruth.split("-").last
  val prediction = label.categoryValue
  val predictionParts = prediction.split("-")
  val predPrefix = predictionParts.head
  val predCategory = predictionParts.last
}

case class Segment(tokens: Seq[Token], label: CitationLabel)

case class ErrorAnalysis(doc: Document) {

  lazy val segments = getSegments

  def analysis(): Unit = {
    val n = segments.length
    var i = 0
    while (i < n) {
      val seg = segments(i)
      i += 1
      
    }
  }

  def getSegments: Seq[Segment] = {
    val segments = new ArrayBuffer[Segment]()
    val tokens = doc.tokens.toIndexedSeq
    val n = tokens.length
    var i = 1
    var lastTag = Label(getTag(tokens.head))
    val chunk = new ArrayBuffer[Token]()
    chunk += tokens.head
    while (i < n) {
      val curr = tokens(i)
      i += 1
      val currLabel = Label(getTag(curr))
      if (currLabel.trueCategory.equals(lastTag.trueCategory)) {
        chunk += curr
      } else {
        segments += Segment(chunk.toSeq, lastTag.label)
        chunk.clear()
        lastTag = Label(currLabel.label)
        chunk += curr
      }
    }
    segments
  }

  def getTag(t: Token): CitationLabel = {
    t.attr[CitationLabel]
  }


}

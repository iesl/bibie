package edu.umass.cs.iesl.bibie

/**
 * Created by kate on 5/13/15.
 */

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.segment.DeterministicTokenizer
import scala.io.Source
import java.io.File
import scala.xml.{NodeSeq, Node}
import scala.collection.mutable.ArrayBuffer

object LoadGrobid {
  def fromFilename(filename: String): Seq[Document] = {
    val buff = new ArrayBuffer[Document]()
    var currDoc = new Document("")
    var currSent = new Sentence(currDoc)
    val lines = Source.fromFile(filename).getLines()
    while (lines.hasNext) {
      val line = lines.next()
      if (line.length > 0) {
        val parts = line.split(" ")
        if (parts.length > 2) {
          val label = {
            val l = parts.last.dropRight(1)
            if (l.startsWith("I-<")) l.drop(3) else l.drop(1)
//            val l = parts.last.trim
//            if (l.startsWith("I-<")) "I-"+l.drop(3)
//            else "B-"+l.drop(1)
          }
          val string = parts.head
          val features = parts.dropRight(1)
          val token = new Token(currSent, string)
          token.attr += new CitationFeatures(token)
          token.attr[CitationFeatures] ++= features
          token.attr += new CitationLabel(label, token)
        }
      } else {
        buff += currDoc
        currDoc = new Document("")
        currSent = new Sentence(currDoc)
      }
    }
    buff
  }
}



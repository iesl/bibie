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

  var total = 0
  val tagSet = new scala.collection.mutable.HashSet[String]()
  val badFiles = new ArrayBuffer[String]()

  case class BibSpan(label: String, contents: String, bib: Node) {
    val children = new ArrayBuffer[BibSpan]()
    def +=(c: Node): Unit = children += BibSpan(c.label, c.text, c)
    def toLabeledTokens: Seq[Token] = {
      val d = new Document("")
      val labelStr =
        if ((bib \ "@type").text.length > 0) label + "_" + (bib \ "@type").text
        else if ((bib \ "@level").text.length > 0) label + "_" + (bib \ "@level").text
        else label
      tagSet += labelStr
      d.appendString(contents)
      DeterministicTokenizer.process(d)
      val tokenSeq = d.tokens.toSeq
      if (tokenSeq.length == 1) {
        tokenSeq.head.attr += new CitationLabel("U-"+labelStr, tokenSeq.head)
      } else if (tokenSeq.length > 1) {
        tokenSeq.head.attr += new CitationLabel("B-"+labelStr, tokenSeq.head)
        tokenSeq.last.attr += new CitationLabel("L-"+labelStr, tokenSeq.last)
        if (tokenSeq.length > 2) {
          tokenSeq.drop(1).dropRight(1).foreach { t => t.attr += new CitationLabel("I-"+labelStr, t) }
        }
      }
      tokenSeq
    }
    override def toString: String = bib.toString() //+ " >> " + children.map(_.toString()).mkString(" ")
  }

  def bibToDoc(bib: Node): Document = {
    val doc = new Document("")
    val sentence = new Sentence(doc)
    // FIXME only goes one level down, make this recursive?
    for (c <- bib.nonEmptyChildren) {
      if (c.label != "#PCDATA" && c.label != "lb") {
        val thisLabel = if (c.label == "notes") "note" else c.label
        val thisNode = BibSpan(thisLabel, c.text, c)
        val tokens = thisNode.toLabeledTokens
        for (token <- tokens) {
          val t = new Token(sentence, token.string)
          t.attr += new CitationLabel(token.attr[CitationLabel].categoryValue, t)
        }
      }
    }
    doc
  }

  def fromFilename(filename: String): Seq[Document] = {
    total += 1
    val docBuff = new ArrayBuffer[Document]()
    val xml = scala.xml.XML.loadFile(new File(filename))
    //try several different xml structures (some files go <xml><citations> ... , some go <xml><biblList> ... , maybe others
    val root = {
      val try1 = xml \ "listBibl"
      if (try1.length > 0) try1 else {
        val try2 = xml \\ "citations"
        try2
      }
    }
    if (root.length <= 0) {
      badFiles += filename
    } else {
      val bibs = root \\ "bibl"
      var i = 0
      while (i < bibs.length) {
        docBuff += bibToDoc(bibs(i))
        i += 1
      }
    }
    docBuff
  }

  def fromDir(dir: String, n: Int = -1): Seq[Document] = {
    val docs = if (n > 0) new File(dir).listFiles.take(n).map(_.getAbsolutePath).flatMap(fromFilename)
    else new File(dir).listFiles.map(_.getAbsolutePath).flatMap(fromFilename)
    println(s"docs with tags: ${tagSet.mkString(", ")}")
    println(s"${badFiles.length}/$total files skipped:")
    badFiles.foreach(println)
    docs
  }
}



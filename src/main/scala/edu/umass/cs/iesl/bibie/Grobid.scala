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
  case class BibSpan(label: String, contents: String, bib: Node) {
    val children = new ArrayBuffer[BibSpan]()
    def +=(c: Node): Unit = children += BibSpan(c.label, c.text, c)
    def toLabeledTokens: Seq[Token] = {
      val d = new Document("")
      val labelStr = if ((bib \ "@type").text.length > 0) label + "_" + (bib \ "@type").text else label
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
      if (c.label != "#PCDATA") {
        val thisNode = BibSpan(c.label, c.text, c)
        val tokens = thisNode.toLabeledTokens
        for (token <- tokens) {
          val t = new Token(sentence, token.string)
          t.attr += new CitationLabel(token.attr[CitationLabel].categoryValue, t)
        }
      }
    }
//    doc.tokens.foreach { t => println(s"${t.string}\t${t.attr[CitationLabel].categoryValue}") }
    doc
  }

  def fromFilename(filename: String): Seq[Document] = {
    println(s"loading: $filename...")
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
    assert(root.length > 0, s"bad xml? $root")
    val bibs = root \\ "bibl"
    var i = 0
    while (i < bibs.length) {
      docBuff += bibToDoc(bibs(i))
      i += 1
    }
    println("done.")
    docBuff
  }
  def fromDir(dir: String, n: Int = -1): Seq[Document] =
    if (n > 0) new File(dir).listFiles.take(n).map(_.getAbsolutePath).flatMap(fromFilename)
    else new File(dir).listFiles.map(_.getAbsolutePath).flatMap(fromFilename)


}

//  def getExtractor(n: NodeSeq, p: String) = Extractor(n, p)
//  def extractor(p: String) = { root: NodeSeq => Extractor(root, p) }

//val elems = Map(
//"author" -> { root: NodeSeq => Extractor(root, "author") }
////    "abstract" -> { root: NodeSeq => Extractor(root, "div>type=abstract") },
////    "author" -> { root: NodeSeq => Extractor(root, "byline>docAuthor") },
////    "affiliation" -> { root: NodeSeq => Extractor(root, "byline>affiliation") },
////    "address" -> { root: NodeSeq => Extractor(root, "address") },
////    "date" -> { root: NodeSeq => Extractor(root, "date") },
////    "email" -> { root: NodeSeq => Extractor(root, "email") },
////    "grant" -> { root: NodeSeq => Extractor(root, "note>type=grant") },
////    "keyword" -> { root: NodeSeq => Extractor(root, "keywords") },
////    "phone" -> { root: NodeSeq => Extractor(root, "phone") }, //or note type=phone?
////    "reference" -> { root: NodeSeq => Extractor(root, "reference") }, //or note type=reference?
////    "submission" -> { root: NodeSeq => Extractor(root, "note>type=submission") },
////    "title" -> { root: NodeSeq => Extractor(root, "docTitle>titlePart") },
////    "web" -> { root: NodeSeq => Extractor(root, "ptr")} // or ptr type=web?
//)
//
//class Extractor(root: NodeSeq) {
//  def apply(pattern: String): Seq[String] = {
//    val parts = pattern.split(">")
//    parts.length match {
//      case 1 => (for (b <- root \\ parts(0)) yield b.text).filter(_.length > 0)
//      case 2 => parts(1).split("=").length match {
//        case 1 => (for (b <- root \\ parts(0)) yield (b \\ parts(1)).text).filter(_.length > 0)
//        case 2 =>
//          val parent = parts(0); val parentType = parts(1).split("=")(1)
//          (root \\ parts(0)).map(elem => (elem \ "@type", elem.text)).filter(_._1.text == parentType).map(_._2)
//      }
//    }
//  }
//}
//
//object Extractor {
//  def apply(root: NodeSeq, pattern: String): Seq[String] = new Extractor(root).apply(pattern)
//}


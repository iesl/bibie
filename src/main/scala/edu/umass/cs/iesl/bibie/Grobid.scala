package edu.umass.cs.iesl.bibie

/**
 * Created by kate on 5/13/15.
 */

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.segment.DeterministicTokenizer
import scala.io.Source
import java.io.File
import scala.xml.{NodeSeq, Node}
import scala.collection.mutable.{ArrayBuffer, HashMap}

/*
M m M M M M M M M M LINESTART ALLCAP NODIGIT 1 0 0 0 0 0 0 0 0 0 0 0 NOPUNCT 1 I-<author>
. . . . . . . . . . LINEIN ALLCAP NODIGIT 1 0 0 0 0 0 0 0 0 0 0 0 DOT 1 <author>
Kitsuregawa kitsuregawa K Ki Kit Kits a wa awa gawa LINEIN INITCAP NODIGIT 0 0 0 0 0 0 0 0 0 0 0 0 NOPUNCT 1 <author>
, , , , , , , , , , LINEIN ALLCAP NODIGIT 1 0 0 0 0 0 0 0 0 0 0 0 COMMA 1 <author>
H h H H H H H H H H LINEIN ALLCAP NODIGIT 1 0 0 0 0 0 0 0 0 0 0 0 NOPUNCT 2 <author>
. . . . . . . . . . LINEIN ALLCAP NODIGIT 1 0 0 0 0 0 0 0 0 0 0 0 DOT 2 <author>
Tanaka tanaka T Ta Tan Tana a ka aka naka LINEIN INITCAP NODIGIT 0 1 0 0 0 0 0 0 0 0 0 0 NOPUNCT 2 <author>
, , , , , , , , , , LINEIN ALLCAP NODIGIT 1 0 0 0 0 0 0 0 0 0 0 0 COMMA 3 <author>
and and a an and and d nd and and LINEIN NOCAPS NODIGIT 0 0 1 0 0 0 0 0 0 0 0 0 NOPUNCT 3 <author>
T t T T T T T T T T LINEIN ALLCAP NODIGIT 1 0 0 0 0 0 0 0 0 0 0 0 NOPUNCT 3 <author>
. . . . . . . . . . LINEIN ALLCAP NODIGIT 1 0 0 0 0 0 0 0 0 0 0 0 DOT 4 <author>
Moto moto M Mo Mot Moto o to oto Moto LINEIN INITCAP NODIGIT 0 0 0 0 0 0 0 0 0 0 0 0 NOPUNCT 4 <author>
- - - - - - - - - - LINEIN ALLCAP NODIGIT 1 0 0 0 0 0 0 0 0 0 0 0 HYPHEN 4 <author>
oka oka o ok oka oka a ka oka oka LINEIN NOCAPS NODIGIT 0 1 0 0 0 0 0 0 0 0 0 0 NOPUNCT 5 <author>
 */

class GoldCitationLabel(val label: String, val token: Token)

object LoadGrobid {
  def fromFilename(filename: String): Seq[Document] = {
    val whitespace = "\\s+".r
    val buff = new ArrayBuffer[Document]()
    var currDoc = new Document("")
    var currSent = new Sentence(currDoc)
    val lines = Source.fromFile(filename).getLines()
    var tokenCount = 0
    var docCount = 0
    while (lines.hasNext) {
      val line = lines.next()
      val parts = whitespace.split(line)
      if (parts.length > 1) {
        val label = {
          val l = parts.last.dropRight(1)
          if (l.startsWith("I-<")) {
            val ll = l.drop(3)
            "B-" + ll
          } else {
            val ll = l.drop(1)
            "I-" + ll
          }
        }
        val string = parts.head
        val features = parts.dropRight(1)
        val token = new Token(currSent, string)
        token.attr += new CitationFeatures(token)
        token.attr[CitationFeatures] ++= features
        token.attr += new CitationLabel(if (!LabelDomain.frozen || LabelDomain.categories.contains(label)) label else "O", token)
        tokenCount += 1
      } else {
        if (currSent.length > 0) currDoc.appendString("")
        if (currDoc.tokenCount > 0) {
          buff += currDoc
          currDoc = new Document("")
          currSent = new Sentence(currDoc)
          docCount += 1
        }
      }
    }
    println(s"Loaded $docCount docs with $tokenCount tokens from file $filename.")
    buff
  }

  def fromFilenameLabeled(filename: String): Seq[Document] = {
    val whitespace = "\\s+".r
    val buff = new ArrayBuffer[Document]()
    var currDoc = new Document("")
    var currSent = new Sentence(currDoc)
    val lines = Source.fromFile(filename).getLines()
    var tokenCount = 0
    var docCount = 0

    assert(lines.nonEmpty, s"no lines loaded from $filename")

    val okayLines = new ArrayBuffer[String]()
    try {
      while (lines.hasNext) okayLines += lines.next()
    } catch {
      case e: Exception => println(e)
    }

    for (line <- okayLines) {
//      val line = lines.next()
      val parts = whitespace.split(line)
      if (parts.length > 1) {
        val guessLabel = {
          val l = parts.last.dropRight(1)
          if (l.startsWith("I-<")) {
            val ll = l.drop(3)
            "B-" + ll
          } else {
            val ll = l.drop(1)
            "I-" + ll
          }
        }
        val trueLabel = {
          val l = parts.dropRight(1).last.dropRight(1)
          if (l.startsWith("I-<")) {
            val ll = l.drop(3)
            "B-" + ll
          } else {
            val ll = l.drop(1)
            "I-" + ll
          }
        }
        val string = parts.head
        val features = parts.dropRight(1)
        val token = new Token(currSent, string)
        token.attr += new CitationFeatures(token)
        token.attr[CitationFeatures] ++= features
        token.attr += new CitationLabel(if (!LabelDomain.frozen || LabelDomain.categories.contains(guessLabel)) guessLabel else "O", token)
        token.attr += new GoldCitationLabel(if (!LabelDomain.frozen || LabelDomain.categories.contains(trueLabel)) trueLabel else "O", token)
        tokenCount += 1
      } else {
        if (currSent.length > 0) currDoc.appendString("")
        if (currDoc.tokenCount > 0) {
          buff += currDoc
          currDoc = new Document("")
          currSent = new Sentence(currDoc)
          docCount += 1
        }
      }
    }

    println(s"Loaded $docCount docs with $tokenCount tokens from file $filename.")
    buff
  }


}

/*
<ref-marker> [30] </ref-marker> <authors> <person> <person-first> E. </person-first> <person-middle> W. </person-middle> <person-last> Montroll , </person-last> </person> <person> <person-first> B. </person-first> <person-middle> J. </person-middle> <person-last> West , </person-last> </person> </authors> <venue> <booktitle> Fluctuation Phenomena , </booktitle> <publisher> Elsevier Science Publishers B. V. , </publisher> <address> Amsterdam , </address> <date> <year> 1979 , </year> </date> <chapter> Ch . On an enriched collection of stochastic processes , </chapter> <pages> pp . 61--205 . </pages> </venue>
 */

/*
  def fromFile(filename: String): Seq[Document] = {
    var document: Document = null
    val documents = new collection.mutable.ArrayBuffer[Document]
    var sentence: Sentence = null

    for (l <- Source.fromFile(filename).getLines()) {
      document = new Document("").setName("NLP-" + documents.length)
      documents += document
      sentence = new Sentence(document)
      val tags = new mutable.HashMap[String, (Int, Int)]()
      var count = 0
      for (t <- l.split("\\s+")) {
        count += 1
        if (t.matches("<[a-z\\-_]*>")) {tags(t.replaceAll("<|>", "")) = (count, 0)}
        else if (t.matches("</[a-z\\-_]*>")) tags.remove(t.replaceAll("<|>|/", ""))
        else {

          // TODO fixme base model is trained on "" not "O"
          val tag = /*if (tags.isEmpty) "O" else */ tags.toSeq.sortBy(_._2._1).map(ta => (if (ta._2._2 == 0) "B-" else "I-") + ta._1).mkString(":")
          if (Exclude.ar.contains(tag)) {
            println("problem with" + tag)
            println("on token" + t)
            println("Here: " + sentence.tokens.map(_.string).mkString(" "))
            println("===========")
          }
          /*if(tag == "I-venue") {
            println("Empty tag: " + tag)
            println("on token" + t)
            println("Here: " + sentence.tokens.map(_.string).mkString(" "))
            println("===========")
          }*/
          if (sentence.length > 0) document.appendString(" ")
          val token = new Token(sentence, t)
          token.attr += new CitationLabel(if (!LabelDomain.frozen || LabelDomain.categories.contains(tag)) tag else "O", token)
          tags.foreach {t => tags(t._1) = (t._2._1, t._2._2 + 1)}
        }
      }
    }
    println("Loaded: " + documents.size + " number of citations.")
    documents.toSeq
  }
 */



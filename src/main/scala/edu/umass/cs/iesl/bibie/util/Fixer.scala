package edu.umass.cs.iesl.bibie.util

import java.io.{File, PrintWriter}

import cc.factorie.app.nlp.{Token, Document, Sentence}
import edu.umass.cs.iesl.bibie.evaluate.SegmentationEvaluation
import edu.umass.cs.iesl.bibie.model.{CitationLabelDomain, CitationLabel}
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

/**
 * Created by kate on 1/2/16.
 */
object Fixer {

  case class Tmp(guess: String)

  def main(args: Array[String]): Unit = {
    implicit val rng = new scala.util.Random(0)
    val infile = args(0)
    val outfile = args(1)
    val docs = load(infile)
    println(s"loaded ${docs.length} docs with ${docs.map(_.tokenCount).sum} tokens")
    CitationLabelDomain.freeze()
    docs.foreach { doc =>
      doc.tokens.foreach { tok =>
        val tmp = tok.attr[Tmp]
        tok.attr[CitationLabel].set(CitationLabelDomain.index(tmp.guess))(null)
        tok.attr.remove[Tmp]
      }
    }
    docs.take(4).foreach { d =>
      d.tokens.foreach {t =>
        val l = t.attr[CitationLabel]
        val corr = if (l.valueIsTarget) "" else "*"
        println(s"$corr ${t.string} ${l.target.categoryValue} ${l.categoryValue}")
      }
      println("")
    }

    val evaluator = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)
    evaluator.printEvaluation(docs, extra = "before")
    evaluator.segmentationEvaluation(docs, extra = "before")
    val fixed = docs.count(fixAuthorSegments)
    println(s"fixed $fixed docs")
    docs.take(4).foreach { d =>
      d.tokens.foreach {t =>
        val l = t.attr[CitationLabel]
        val corr = if (l.valueIsTarget) "" else "*"
        println(s"$corr ${t.string} ${l.target.categoryValue} ${l.categoryValue}")
      }
      println("")
    }
    evaluator.printEvaluation(docs, extra = "fixed")
    evaluator.segmentationEvaluation(docs, extra = "fixed")
    writeDecisions(docs, outfile)
  }

  def load(filename: String)(implicit rng: scala.util.Random): Seq[Document] = {
    val docs = new ArrayBuffer[Document]()
    var doc = new Document()
    var sent = new Sentence(doc)
    val lines = Source.fromFile(filename).getLines()
    while (lines.hasNext) {
      val line = lines.next()
      if (line.length == 0) {
        if (doc.tokenCount > 0) {
          docs += doc
          doc = new Document()
          sent = new Sentence(doc)
        }
      } else {
        val parts = line.split(" ")
        assert(parts.length == 3)
        val str = parts(0)
        val truth = parts(1)
        val guess = parts(2).trim
        val token = new Token(sent, str)
        token.attr += new CitationLabel(truth, token)
        token.attr += new Tmp(guess)
      }
    }
//    CitationLabelDomain.freeze()
//    docs.foreach { doc =>
//      doc.tokens.foreach { tok =>
//        val tmp = tok.attr[Tmp]
//        tok.attr[CitationLabel].setRandomly(rng)
////        tok.attr[CitationLabel].set(CitationLabelDomain.index(tmp.guess))(null)
//        tok.attr.remove[Tmp]
//      }
//    }
    docs.toSeq
  }


  def writeDecisions(docs: Seq[Document], outfile: String): Unit = {
    val pw = new PrintWriter(new File(outfile))
    for (doc <- docs; sent <- doc.sentences if sent.tokens.nonEmpty) {
      for (token <- sent.tokens) {
        val string = token.string
        val truth = token.attr[CitationLabel].target.categoryValue
        val guess = token.attr[CitationLabel].categoryValue
        val line = s"$string $truth $guess\n"
        pw.write(line)
      }
      pw.write("\n")
    }
    pw.close()
  }

  def fixAuthorSegments(doc: Document): Boolean = {
    var fixed = false
    val sb = new StringBuilder()
    doc.tokens.foreach { token =>
      val label = token.attr[CitationLabel]
      sb.append(s"${label.target.categoryValue}\t${label.categoryValue}\t${token.string}\n")
    }
    val tokens: Seq[Token] = doc.tokens.toSeq
    val tags = doc.tokens.map(_.attr[CitationLabel])
    val authors = tags.zipWithIndex.filter { case (tag, idx) =>
      val parts = tag.categoryValue.split("-")
      val prefix = parts.head
      val base = parts.last
      prefix.equals("I") && base.equals("author")
    }
    if (authors.nonEmpty) {
      val authorTokens: Seq[Token] = authors.map { case (tag, idx) => tokens(idx) }.toSeq
      val lastTok: Token = authorTokens.sortBy { t => t.position }.last
      if (lastTok.hasNext) {
        val next = lastTok.next
        if (next.string.equals(".")) {
          fixed = true
          next.attr[CitationLabel].set(CitationLabelDomain.index("I-author"))(null)
        }
      }
    }
    fixed
  }
}

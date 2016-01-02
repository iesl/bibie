package edu.umass.cs.iesl.bibie.util

import java.io.{File, PrintWriter}

import cc.factorie.app.nlp.{Token, Document}
import edu.umass.cs.iesl.bibie.evaluate.SegmentationEvaluation
import edu.umass.cs.iesl.bibie.load.LoadGrobid
import edu.umass.cs.iesl.bibie.model.{CitationLabelDomain, CitationLabel}

/**
 * Created by kate on 12/30/15.
 */
object EvalGrobid {

  def main(args: Array[String]): Unit = {
    val resultsFile = args(0)
    val outputFile = args(1)
    val docs = LoadGrobid.fromFilenameWithResults2(resultsFile)
    val evaluator = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)
    evaluator.printEvaluation(docs, extra = "GROBID (before fix)")
    evaluator.segmentationEvaluation(docs, extra = "GROBID (before fix)")
    writeDecisions(docs, outputFile)
    val fixes = docs.count(fixAuthorSegments)
    println("")
    println("evaluation after fixing author segments")
    evaluator.printEvaluation(docs, extra = s"GROBID (fixed)")
    evaluator.segmentationEvaluation(docs, extra = s"GROBID (fixed)")
    println(s"\nfixed $fixes documents")
    writeDecisions(docs, outputFile + ".fixed")
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
//    if (fixed) {
//      println("before:")
//      println(sb.toString())
//      println("after:")
//      doc.tokens.foreach { token =>
//        val label = token.attr[CitationLabel]
//        println(s"${label.target.categoryValue}\t${label.categoryValue}\t${token.string}")
//      }
//    }
    fixed
  }

}

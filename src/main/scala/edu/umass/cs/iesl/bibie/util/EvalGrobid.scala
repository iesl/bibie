package edu.umass.cs.iesl.bibie.util

import java.io.{File, PrintWriter}

import cc.factorie.app.nlp.Document
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
    evaluator.printEvaluation(docs, extra = "GROBID")
    evaluator.segmentationEvaluation(docs, extra = "GROBID")
    writeDecisions(docs, outputFile)
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

}

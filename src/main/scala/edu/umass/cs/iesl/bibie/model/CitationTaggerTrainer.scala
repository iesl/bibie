package edu.umass.cs.iesl.bibie.model

/**
 * Created by kate on 10/13/15.
 */

import cc.factorie.app.nlp._
import cc.factorie.util.HyperparameterMain

import edu.umass.cs.iesl.bibie.BibieOptions
import edu.umass.cs.iesl.bibie.evaluate.SegmentationEvaluation
import edu.umass.cs.iesl.bibie.load._
import edu.umass.cs.iesl.bibie.segment.OverSegmenter

import java.util.logging.Logger
import java.io._

object CitationTaggerTrainer extends HyperparameterMain {

  private val logger = Logger.getLogger(getClass.getName)

  def evaluateParameters(args: Array[String]): Double = {
    val opts = new BibieOptions
    opts.parse(args)
    println("ARGS:")
    println(opts.unParse.mkString("\n"))
    opts.taggerType.value match {
      case "default" => trainDefault(opts)
      case "grobid" => trainGrobid(opts)
      case "combined" => trainCombined(opts)
      case _ => throw new Exception(s"invalid tagger type: ${opts.taggerType.value}")
    }
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

  def trainDefault(opts: BibieOptions): Double = {
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val useGrobid = opts.useGrobid.value
    val trainDocs = if (useGrobid) LoadGrobid.fromFilename(opts.trainFile.value) else LoadHier.fromFile(opts.trainFile.value)
    val devDocs = if (useGrobid) LoadGrobid.fromFilename(opts.testFile.value) else LoadHier.fromFile(opts.devFile.value)
    val lexiconDir = opts.lexiconUrl.value
    OverSegmenter.overSegment(trainDocs ++ devDocs, lexiconDir)
    val tagger = new DefaultCitationTagger(lexiconDir)
    val trainEval = tagger.train(trainDocs, devDocs, params)
    logger.info(s"train eval: $trainEval")
    devDocs.foreach(tagger.process)
    writeDecisions(devDocs, opts.outputFile.value)
    if (opts.saveModel.value) {
      logger.info(s"serializing model to ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }
    if (opts.fixAuthorSegments.value) {
      val fixes = devDocs.count(fixAuthorSegments)
      println("")
      println("evaluation after fixing author segments")
      val evaluator = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)
      evaluator.printEvaluation(devDocs, extra = s"dev (fixed)")
      evaluator.segmentationEvaluation(devDocs, extra = s"dev (fixed)")
      println(s"\nfixed $fixes documents")
      writeDecisions(devDocs, opts.outputFile.value + ".fixed")
    }
    trainEval
  }

  def trainGrobid(opts: BibieOptions): Double = {
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val tagger = new GrobidCitationTagger
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value)
    val devDocs = LoadGrobid.fromFilename(opts.devFile.value)
    val trainEval = tagger.train(trainDocs, devDocs, params)
    logger.info(s"train eval: $trainEval")
    devDocs.foreach(tagger.process)
    writeDecisions(devDocs, opts.outputFile.value)
    if (opts.saveModel.value) {
      logger.info(s"serializing model to ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }
    trainEval
  }

  def trainCombined(opts: BibieOptions): Double = {
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value)
    val devDocs = LoadGrobid.fromFilename(opts.devFile.value)
    val lexiconDir = opts.lexiconUrl.value
    OverSegmenter.overSegment(trainDocs ++ devDocs, lexiconDir)
    val tagger = new CombinedCitationTagger(lexiconDir)
    val trainEval = tagger.train(trainDocs, devDocs, params)
    logger.info(s"train eval: $trainEval")
    devDocs.foreach(tagger.process)
    writeDecisions(devDocs, opts.outputFile.value)
    if (opts.saveModel.value) {
      logger.info(s"serializing model to ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }
    trainEval
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

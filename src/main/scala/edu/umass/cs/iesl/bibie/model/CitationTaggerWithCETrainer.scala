package edu.umass.cs.iesl.bibie.model

import java.io.{PrintWriter, FileOutputStream, File}
import java.util.logging.Logger

import cc.factorie.util.HyperparameterMain
import cc.factorie.app.nlp._

import edu.umass.cs.iesl.bibie.BibieOptions
import edu.umass.cs.iesl.bibie.load.{LoadHier, LoadGrobid}
import edu.umass.cs.iesl.bibie.segment.OverSegmenter

/**
 * Created by kate on 12/28/15.
 */
object CitationTaggerWithCETrainer extends HyperparameterMain {

  private val logger = Logger.getLogger(getClass.getName)

  def evaluateParameters(args: Array[String]): Double = {
    val opts = new BibieOptions
    opts.parse(args)
    println("ARGS:")
    println(opts.unParse.mkString(", "))
    opts.taggerType.value match {
      case "default" => trainDefault(opts)
      case "grobid" => trainGrobid(opts)
      case "combined" => trainCombined(opts)
      case _ => throw new Exception(s"invalid tagger type: ${opts.taggerType.value}")
    }
  }

  def trainDefault(opts: BibieOptions): Double = {
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val useGrobid = opts.useGrobid.value
    val trainDocs = if (useGrobid) LoadGrobid.fromFilename(opts.trainFile.value) else LoadHier.fromFile(opts.trainFile.value)
    val devDocs = if (useGrobid) LoadGrobid.fromFilename(opts.testFile.value) else LoadHier.fromFile(opts.devFile.value)
    val lexiconDir = opts.lexiconUrl.value
    OverSegmenter.overSegment(trainDocs ++ devDocs, lexiconDir)
    println(s"loading embeddings from ${opts.embeddingsFile.value}")
    val embeddingMap = CharEmbedding(opts.embeddingsFile.value, opts.vocabFile.value, opts.embeddingDim.value)
    val tagger = new DefaultCitationTaggerWithCE(lexiconDir,
      embeddingMap,
      opts.embeddingDim.value,
      opts.scale.value,
      useOffsetEmbedding = true)
    val trainEval = tagger.train(trainDocs, devDocs, params)
    logger.info(s"train eval: $trainEval")
    devDocs.foreach(tagger.process)
    writeDecisions(devDocs, opts.outputFile.value)
//    if (opts.saveModel.value) {
//      logger.info(s"serializing model to ${opts.modelFile.value}")
//      tagger.serialize(new FileOutputStream(opts.modelFile.value))
//    }
    trainEval
  }

  def trainGrobid(opts: BibieOptions): Double = {
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    println(s"loading embeddings from ${opts.embeddingsFile.value}")
    val embeddingMap = CharEmbedding(opts.embeddingsFile.value, opts.vocabFile.value, opts.embeddingDim.value)
    val tagger = new GrobidCitationTaggerWithCE(
      embeddingMap,
      opts.embeddingDim.value,
      opts.scale.value,
      useOffsetEmbedding = true)
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value)
    val devDocs = LoadGrobid.fromFilename(opts.devFile.value)
    val trainEval = tagger.train(trainDocs, devDocs, params)
    logger.info(s"train eval: $trainEval")
    devDocs.foreach(tagger.process)
    writeDecisions(devDocs, opts.outputFile.value)
    //    if (opts.saveModel.value) {
    //      logger.info(s"serializing model to ${opts.modelFile.value}")
    //      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    //    }
    trainEval
  }

  def trainCombined(opts: BibieOptions): Double = {
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value)
    val devDocs = LoadGrobid.fromFilename(opts.devFile.value)
    val lexiconDir = opts.lexiconUrl.value
    OverSegmenter.overSegment(trainDocs ++ devDocs, lexiconDir)
    println(s"loading embeddings from ${opts.embeddingsFile.value}")
    val embeddingMap = CharEmbedding(opts.embeddingsFile.value, opts.vocabFile.value, opts.embeddingDim.value)
    val tagger = new CombinedCitationTaggerWithCE(lexiconDir,
      embeddingMap,
      opts.embeddingDim.value,
      opts.scale.value,
      useOffsetEmbedding = true)
    val trainEval = tagger.train(trainDocs, devDocs, params)
    logger.info(s"train eval: $trainEval")
    devDocs.foreach(tagger.process)
    writeDecisions(devDocs, opts.outputFile.value)
//    if (opts.saveModel.value) {
//      logger.info(s"serializing model to ${opts.modelFile.value}")
//      tagger.serialize(new FileOutputStream(opts.modelFile.value))
//    }
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


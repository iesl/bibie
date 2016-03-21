package edu.umass.cs.iesl.bibie.model

/**
 * Created by kate on 10/13/15.
 */

import java.io._

import cc.factorie.app.nlp.Document
import cc.factorie.util.HyperparameterMain
import edu.umass.cs.iesl.bibie.BibieOptions
import edu.umass.cs.iesl.bibie.load._
import edu.umass.cs.iesl.bibie.segment.OverSegmenter

object CitationTaggerTrainer extends HyperparameterMain {

  implicit val random = new scala.util.Random(0)

  def evaluateParameters(args: Array[String]): Double = {
    val opts = new BibieOptions
    opts.parse(args)
    opts.taggerType.value match {
      case "default" => trainDefault(opts)
      case "grobid" => trainGrobid(opts)
      case "combined" => trainCombined(opts)
      case _ => throw new Exception(s"invalid tagger type: ${opts.taggerType.value}")
    }
  }

  def trainDefault(opts: BibieOptions): Double = {
    val logOpt = getLogOpt(opts.logFile.value)
    val lexiconDir = opts.lexiconUrl.value
    val (trainDocs, devDocs) = loadData(opts)
    val tagger = new DefaultCitationTagger(logOpt, lexiconDir)
    val params = new Hyperparams(opts)
    OverSegmenter.overSegment(trainDocs ++ devDocs, lexiconDir)
    val trainEval = tagger.train(trainDocs, devDocs, params)
    tagger.log.info(s"train eval: $trainEval")
    if (opts.saveModel.value) {
      tagger.log.info(s"serializing model to ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }
    trainEval
  }

  def trainGrobid(opts: BibieOptions): Double = {
    val logOpt = getLogOpt(opts.logFile.value)
    val (trainDocs, devDocs) = loadData(opts)
    val tagger = new GrobidCitationTagger(logOpt)
    val params = new Hyperparams(opts)
    val trainEval = tagger.train(trainDocs, devDocs, params)
    tagger.log.info(s"train eval: $trainEval")
    if (opts.saveModel.value) {
      tagger.log.info(s"serializing model to ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }
    trainEval
  }

  def trainCombined(opts: BibieOptions): Double = {
    val logOpt = getLogOpt(opts.logFile.value)
    val lexiconDir = opts.lexiconUrl.value
    val (trainDocs, devDocs) = loadData(opts)
    val tagger = new CombinedCitationTagger(logOpt, lexiconDir)
    val params = new Hyperparams(opts)
    OverSegmenter.overSegment(trainDocs ++ devDocs, lexiconDir)
    val trainEval = tagger.train(trainDocs, devDocs, params)
    tagger.log.info(s"train eval: $trainEval")
    if (opts.saveModel.value) {
      tagger.log.info(s"serializing model to ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }
    trainEval
  }

  /*

  Helpers

   */
  def loadDocs(filename: String, dataType: String, n: Int = -1): Seq[Document] = {
    dataType match {
      case "grobid" => LoadGrobid.fromFilename(filename)
      case "iesl" => LoadHier.fromFile(filename)
      case _ => throw new Exception(s"invalid data type: $dataType")
    }
  }

  def loadData(opts: BibieOptions): (Seq[Document], Seq[Document]) = {
    if (opts.devFile.wasInvoked && !opts.devFile.value.equals("")) {
      val train = loadDocs(opts.trainFile.value, opts.dataType.value)
      val dev = loadDocs(opts.devFile.value, opts.dataType.value)
      (train, dev)
    } else {
      val allDocs = loadDocs(opts.trainFile.value, opts.dataType.value)
      splitData(allDocs)
    }
  }

  def splitData(docs: Seq[Document], trainPortion: Double = 0.8)(implicit random: scala.util.Random): (Seq[Document], Seq[Document]) = {
    val shuff = random.shuffle(docs)
    val n = docs.length
    val ntrain = math.floor(trainPortion * n).toInt
    val train = shuff.take(ntrain)
    val dev = shuff.drop(ntrain)
    (train, dev)
  }

  def getLogOpt(logFilename: String): Option[String] = logFilename match {
      case "" => None
      case _ => Some(logFilename)
  }

}

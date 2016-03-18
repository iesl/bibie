package edu.umass.cs.iesl.bibie.model

/**
 * Created by kate on 10/13/15.
 */

import cc.factorie.app.nlp.Document
import cc.factorie.util.HyperparameterMain

import edu.umass.cs.iesl.bibie.BibieOptions
import edu.umass.cs.iesl.bibie.load._
import edu.umass.cs.iesl.bibie.segment.OverSegmenter

import java.util.logging.Logger
import java.io._

object CitationTaggerTrainer extends HyperparameterMain {

  private val logger = Logger.getLogger(getClass.getName)

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
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val (trainDocs, devDocs) = loadData(opts)
    val lexiconDir = opts.lexiconUrl.value
    OverSegmenter.overSegment(trainDocs ++ devDocs, lexiconDir)
    val tagger = new DefaultCitationTagger(lexiconDir)
    val trainEval = tagger.train(trainDocs, devDocs, params)
    logger.info(s"train eval: $trainEval")
    if (opts.saveModel.value) {
      logger.info(s"serializing model to ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }
    trainEval
  }

  def trainGrobid(opts: BibieOptions): Double = {
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val tagger = new GrobidCitationTagger
    val (trainDocs, devDocs) = loadData(opts)
    val trainEval = tagger.train(trainDocs, devDocs, params)
    logger.info(s"train eval: $trainEval")
    if (opts.saveModel.value) {
      logger.info(s"serializing model to ${opts.modelFile.value}")
      tagger.serialize(new FileOutputStream(opts.modelFile.value))
    }
    trainEval
  }

  def trainCombined(opts: BibieOptions): Double = {
    implicit val random = new scala.util.Random(0)
    val params = new Hyperparams(opts)
    val (trainDocs, devDocs) = loadData(opts)
    val lexiconDir = opts.lexiconUrl.value
    OverSegmenter.overSegment(trainDocs ++ devDocs, lexiconDir)
    val tagger = new CombinedCitationTagger(lexiconDir)
    val trainEval = tagger.train(trainDocs, devDocs, params)
    logger.info(s"train eval: $trainEval")
    if (opts.saveModel.value) {
      logger.info(s"serializing model to ${opts.modelFile.value}")
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

  def splitData(docs: Seq[Document], trainPortion: Double = 0.8): (Seq[Document], Seq[Document]) = {
    val n = docs.length
    val ntrain = math.floor(trainPortion * n).toInt
    val train = docs.take(ntrain)
    val dev = docs.drop(ntrain)
    (train, dev)
  }

}

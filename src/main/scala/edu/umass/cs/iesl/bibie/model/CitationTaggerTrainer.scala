package edu.umass.cs.iesl.bibie.model

/**
 * Created by kate on 10/13/15.
 */

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
    val trainDocs = LoadHier.fromFile(opts.trainFile.value)
    val devDocs = LoadHier.fromFile(opts.devFile.value)
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
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value)
    val devDocs = LoadGrobid.fromFilename(opts.devFile.value)
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
    val trainDocs = LoadGrobid.fromFilename(opts.trainFile.value)
    val devDocs = LoadGrobid.fromFilename(opts.devFile.value)
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

}

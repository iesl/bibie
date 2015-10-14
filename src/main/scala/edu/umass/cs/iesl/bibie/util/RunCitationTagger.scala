package edu.umass.cs.iesl.bibie.util

/**
 * Created by kate on 10/14/15.
 */

import cc.factorie.app.nlp.Document
import edu.umass.cs.iesl.bibie.BibieOptions
import edu.umass.cs.iesl.bibie.load.{LoadGrobid, LoadHier}
import edu.umass.cs.iesl.bibie.model.{CombinedCitationTagger, GrobidCitationTagger, Hyperparams, DefaultCitationTagger}
import edu.umass.cs.iesl.bibie.segment.OverSegmenter

object RunCitationTagger {

  def main(args: Array[String]): Unit = {
    val opts = new BibieOptions
    opts.parse(args)
    opts.taggerType.value match {
      case "default" => runDefault(opts)
      case "grobid" => runGrobid(opts)
      case "combined" => runCombined(opts)
      case _ => throw new Exception(s"invalid tagger type: ${opts.taggerType.value}")
    }
  }

  def runDefault(opts: BibieOptions): Seq[Document] = {
    val docs = LoadHier.fromFile(opts.testFile.value)
    val lexiconDir = opts.lexiconUrl.value
    OverSegmenter.overSegment(docs, lexiconDir)
    val tagger = new DefaultCitationTagger(lexiconDir, opts.modelFile.value)
    docs.map(tagger.process)
  }

  def runGrobid(opts: BibieOptions): Seq[Document] = {
    val docs = LoadGrobid.fromFilename(opts.testFile.value)
    val tagger = new GrobidCitationTagger
    docs.map(tagger.process)
  }

  def runCombined(opts: BibieOptions): Seq[Document] = {
    val docs = LoadGrobid.fromFilename(opts.testFile.value)
    val lexiconDir = opts.lexiconUrl.value
    OverSegmenter.overSegment(docs, lexiconDir)
    val tagger = new CombinedCitationTagger(lexiconDir, opts.modelFile.value)
    docs.map(tagger.process)
  }

}

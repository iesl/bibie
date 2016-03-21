package edu.umass.cs.iesl.bibie

import edu.umass.cs.iesl.bibie.model._
import edu.umass.cs.iesl.bibie.segment.OverSegmenter


/**
 * Created by kate on 3/1/16.
 */
object CitationTaggerRunner {

  def main(args: Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    val opts = new BibieOptions
    opts.parse(args)
    val logOpt = CitationTaggerTrainer.getLogOpt(opts.logFile.value)
    val lexiconDir = opts.lexiconUrl.value
    val tagger = opts.taggerType.value match {
      case "grobid" => new GrobidCitationTagger(logOpt)
      case "combined" => new CombinedCitationTagger(logOpt, lexiconDir)
      case "default" => new DefaultCitationTagger(logOpt, lexiconDir)
    }

    tagger.log.info(s"label domain size: ${CitationLabelDomain.size}")
    tagger.log.info(s"feature domain size: ${tagger.FeatureDomain.dimensionSize}")

    val docs = CitationTaggerTrainer.loadDocs(opts.testFile.value, opts.dataType.value)

    docs.take(2).foreach { doc =>
      doc.tokens.take(5).foreach { tok =>
        tagger.log.info(s"${tok.string} ${tok.attr[CitationLabel].toString()}")
      }
    }

    tagger.log.info(s"loaded ${docs.length} docs with ${docs.map(_.tokenCount).sum} tokens")

    val labels = docs.flatMap(_.tokens).map(_.attr[CitationLabel]).toIndexedSeq
    OverSegmenter.overSegment(docs, lexiconDir)
    docs.foreach(tagger.process)
    tagger.evaluation(labels, opts.segmentScheme.value)
  }

}

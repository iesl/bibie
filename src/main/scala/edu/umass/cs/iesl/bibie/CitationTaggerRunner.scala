package edu.umass.cs.iesl.bibie

import java.util.logging.Logger

import edu.umass.cs.iesl.bibie.model._
import cc.factorie.app.nlp.lexicon.StaticLexicons
import edu.umass.cs.iesl.bibie.segment.OverSegmenter


/**
 * Created by kate on 3/1/16.
 */
object CitationTaggerRunner {

  private val log = Logger.getLogger(getClass.getName)

  def main(args: Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    val opts = new BibieOptions
    opts.parse(args)
    val params = new Hyperparams(opts)
    val lexiconDir = opts.lexiconUrl.value
    val tagger = opts.taggerType.value match {
      case "grobid" => new GrobidCitationTagger
      case "combined" => new CombinedCitationTagger(lexiconDir)
      case "default" => new DefaultCitationTagger(lexiconDir)
    }

    log.info(s"label domain size: ${CitationLabelDomain.size}")
    log.info(s"feature domain size: ${CitationFeaturesDomain.dimensionSize}")

    val docs = CitationTaggerTrainer.loadDocs(opts.testFile.value, opts.dataType.value)

    docs.take(2).foreach { doc =>
      doc.tokens.take(5).foreach { tok =>
        log.info(s"${tok.string} ${tok.attr[CitationLabel].toString()}")
      }
    }

    log.info(s"loaded ${docs.length} docs with ${docs.map(_.tokenCount).sum} tokens")

    val labels = docs.flatMap(_.tokens).map(_.attr[CitationLabel]).toIndexedSeq
    OverSegmenter.overSegment(docs, lexiconDir)
    docs.foreach(tagger.process)
    tagger.evaluation(labels, opts.segmentScheme.value)
  }

}

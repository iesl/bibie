package edu.umass.cs.iesl.bibie

import cc.factorie.util._
import cc.factorie.app.nlp.Document

import edu.umass.cs.iesl.bibie.model._
import edu.umass.cs.iesl.bibie.load.IOHelper
import edu.umass.cs.iesl.bibie.segment.{OverSegmenter, CitationSpanList}

import scala.io.StdIn
import scala.collection.mutable

import java.util.logging.Logger

/**
 * @author Kate Silverstein 
 *         created on 9/12/15
 */

class State {
  val docs = new mutable.ArrayBuffer[Document]()
  var idx: Int = 0
  var model: CitationCRFModel = null
}

object Cli {

  private val logger = Logger.getLogger(getClass.getName)

  val opts = new BibieOptions
  val state = new State

  def main(args: Array[String]): Unit = {
    opts.parse(args)
    print("> ")
    var text = StdIn.readLine()
    while (text != null) {
      logger.info(text)
      text match {
        case "load" => loadDocs()
        case "load-model" => loadModel()

        case "curr" => printDoc()
        case "next" =>
          if (state.idx < state.docs.length - 1) state.idx += 1
          printDoc()
        case "prev" =>
          if (state.idx > 0) state.idx -= 1
          printDoc()

        case "xml" => printXML()
        case "tokens" => printTokens()

        case "process" => processDocs()
        case "eval" => evaluate()

        case "segment" => oversegment()
        case "train" => TrainCitationModel.trainModel(opts)
        case "test" => TestCitationModel.testModel(opts)

        case "quit" => System.exit(0)
        case _ => println("???")
      }
      print("> ")
      text = StdIn.readLine()
    }
  }

  def processDocs(): Unit = {
    if (state.model == null) {
      logger.info("loading model from " + opts.modelFile.value)
      loadModel()
    }
    oversegment()
    val ann = new BibieAnnotator(state.model)
    state.docs.foreach(ann.process)
  }

  def evaluate(): Unit = {
    import edu.umass.cs.iesl.bibie.evaluate.SegmentationEvaluation
    implicit val random = new scala.util.Random(0)
    val evaluator = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)
    val labels: Seq[CitationLabel] = state.docs.flatMap(_.tokens.map(_.attr[CitationLabel]))
    labels.foreach(_.setRandomly)
    processDocs()
    evaluator.printEvaluation(state.docs, "DEV (printEvaluation)")
    evaluator.segmentationEvaluation(state.docs, "DEV (segmentationEvaluation)")
  }

  def loadDocs(): Unit = {
    import edu.umass.cs.iesl.bibie.load.LoadHier
    state.docs ++= LoadHier.fromFile(opts.devFile.value)
    logger.info(s"loaded ${state.docs.length} documents")
  }

  def loadModel(): Unit = {
    val model = IOHelper.deserializeModel(opts.modelFile.value, opts.lexiconUrl.value)
    state.model = model
  }

  def printDoc(): Unit = {
    val doc = state.docs(state.idx)
    println(doc.name)
    println(doc.tokens.size)
    if (doc.attr[CitationSpanList] != null) {
      println("Citation Span List:")
      val csl = doc.attr[CitationSpanList]
      for (c <- csl.spans) {
        println(s"\t${c.toString()}")
      }
    }
    println("\t\t*\t*\t*")
    println("tokens:")
    for (t <- doc.tokens) {
      println(s"${t.string}\t${t.attr.toString()}")
    }
  }

  def printTokens(): Unit = {
    val doc = state.docs(state.idx)
    for (t <- doc.tokens) {
      val label = t.attr[CitationLabel].categoryValue
      val gold = t.attr[CitationLabel].target.categoryValue
      val mistake = if (label.equals(gold)) "" else "*"
      println(s"$mistake\t${t.string}\t$label\t$gold")
    }
  }

  def printXML(): Unit = {
    val doc = state.docs(state.idx)
    println(doc.toXML())
  }

  def oversegment(): Unit = {
    OverSegmenter.overSegment(state.docs, opts.lexiconUrl.value)
  }
}

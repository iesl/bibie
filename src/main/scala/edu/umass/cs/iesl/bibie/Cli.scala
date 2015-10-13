//package edu.umass.cs.iesl.bibie
//
//import cc.factorie.util._
//import cc.factorie.app.nlp.Document
//import cc.factorie.app.chain.SegmentEvaluation
//
//import edu.umass.cs.iesl.bibie.model._
//import edu.umass.cs.iesl.bibie.load.{LoadGrobid, IOHelper}
//import edu.umass.cs.iesl.bibie.segment.{OverSegmenter, CitationSpanList}
//
//import scala.io.StdIn
//import scala.collection.mutable
//
//import java.util.logging.Logger
//import java.time
//import edu.umass.cs.iesl.bibie.evaluate.SegmentationEvaluation
//
///**
// * @author Kate Silverstein
// *         created on 9/12/15
// */
//
//class State {
//  val docs = new mutable.ArrayBuffer[Document]()
//  var idx: Int = 0
//  var model: CitationCRFModel = null
//}
//
//object Cli {
//
//  private val logger = Logger.getLogger(getClass.getName)
//
//  val opts = new BibieOptions
//  val state = new State
//
//  def main(args: Array[String]): Unit = {
//    opts.parse(args)
//    print("> ")
//    var text = StdIn.readLine()
//    while (text != null) {
//      logger.info(text)
//      val parts = text.split(" ")
//      if (parts.length == 1) {
//        val arg1 = parts.head
//        arg1 match {
//          case "load" => loadDocs()
//          case "load-model" => loadModel()
//
//          case "curr" => printDoc()
//          case "next" =>
//            if (state.idx < state.docs.length - 1) state.idx += 1
//            printDoc()
//          case "prev" =>
//            if (state.idx > 0) state.idx -= 1
//            printDoc()
//
//          case "xml" => printXML()
//          case "tokens" => printTokens()
//          case "spans" => printSpans()
//
//          case "process" => processDocs()
//          case "eval" => evaluate()
//
//          case "segment" => oversegment()
//
//          case "quick" => quickTest()
//
////          //train on umass-citation dataset
////          case "train" => TrainCitationModel.trainModel(opts)
////          case "test" => TestCitationModel.testModel(opts)
////
////          // train on grobid dataset
////          case "train-grobid-using-umass-features" => TrainCitationModel.trainModelUmassFeatures(opts)
////          case "train-grobid-using-grobid-features" => TrainCitationModel.trainModelGrobidFeaturesOnly(opts)
////          case "train-grobid-using-both" => TrainCitationModel.trainModelBothFeatureSets(opts)
//
//          case "quit" => System.exit(0)
//          case _ => println("???")
//        }
//      } else if (parts.length == 3) {
//        val arg1 = parts(0)
//        val arg2 = parts(1)
//        val arg3 = parts(2)
//        arg1 match {
//          case "train" =>
//            opts.dataSet.setValue(arg2)
//            opts.featureSet.setValue(arg3)
//            val dateStr: String = {
//              val d = time.LocalDateTime.now()
//              d.toString.replaceAll(":", "_")
//            }
//            val modelName = s"${opts.rootPath.value}/bibie.factorie.$arg2.$arg3.$dateStr"
//            opts.modelFile.setValue(modelName)
//            logger.info(s"dataSet: $arg2, featureSet: $arg3, modelName: $modelName")
//            TrainCitationModel.run(opts)
//          case "test" =>
//            if (arg2.equals("grobid")) {
//              val evaluator = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)
//              state.docs ++= LoadGrobid.fromFilename(opts.testFile.value)
//              loadModel()
//              val ann = new BibieAnnotator(state.model, opts.lexiconUrl.value)
//              state.docs.foreach(ann.processBoth)
//              evaluator.printEvaluation(state.docs, "UMASS")
//              evaluator.segmentationEvaluation(state.docs, "UMASS (SegmentationEvaluation)")
//            } else {
//              throw new Exception("not supported yet")
//            }
//
//
//        }
//      } else {
//        println(s"invalid: $text")
//      }
//      print("> ")
//      text = StdIn.readLine()
//    }
//  }
//
//  def quickTest(): Unit = {
//    implicit val random = new scala.util.Random(0)
//    val fname = "/home/kate/AI2/bibie/grobid-results.txt"
//    val evaluator = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)
//
//    if (state.model == null) loadModel()
//    state.docs.clear()
//
//    state.docs ++= LoadGrobid.fromFilenameWithGrobidResults(fname)
//    val labels = state.docs.flatMap(_.tokens.map(_.attr[CitationLabel]))
//    val eval = new SegmentEvaluation[CitationLabel]("B-", "I-", CitationLabelDomain, labels.toIndexedSeq)
//    println(eval)
//    val t = eval("author")
//    println(t.f1)
////    state.docs.foreach { doc =>
////      doc.tokens.foreach { token =>
////        val label = token.attr[CitationLabel]
////        token.attr.remove[CitationLabel]
////        val newLabel = new CitationLabel(label.target.categoryValue, token)
////        token.attr += newLabel
////      }
////    }
////    val ann = new BibieAnnotator(state.model, opts.lexiconUrl.value)
////    state.docs.foreach(ann.processGrobid)
////    evaluator.printEvaluation(state.docs, "UMASS")
////    evaluator.segmentationEvaluation(state.docs, "UMASS (SegmentationEvaluation)")
////    println("")
////
////    val docs = LoadGrobid.fromFilenameWithGrobidResults(fname)
////    evaluator.printEvaluation(docs, "GROBID")
////    evaluator.segmentationEvaluation(docs, "GROBID (SegmentationEvaluation)")
//
//
//  }
//
//  def processDocs(): Unit = {
//    if (state.model == null) {
//      logger.info("loading model from " + opts.modelFile.value)
//      loadModel()
//    }
////    oversegment()
//    val ann = new BibieAnnotator(state.model, opts.lexiconUrl.value)
//    state.docs.foreach(ann.process)
//  }
//
//  def evaluate(): Unit = {
//    import edu.umass.cs.iesl.bibie.evaluate.SegmentationEvaluation
//    implicit val random = new scala.util.Random(0)
//    val evaluator = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)
//    val labels: Seq[CitationLabel] = state.docs.flatMap(_.tokens.map(_.attr[CitationLabel]))
//    labels.foreach(_.setRandomly)
//    processDocs()
//    evaluator.printEvaluation(state.docs, "DEV (printEvaluation)")
//    evaluator.segmentationEvaluation(state.docs, "DEV (segmentationEvaluation)")
//  }
//
//  def loadDocs(): Unit = {
//    import edu.umass.cs.iesl.bibie.load.LoadHier
//    state.docs ++= LoadHier.fromFile(opts.devFile.value)
//    logger.info(s"loaded ${state.docs.length} documents")
//  }
//
//  def loadModel(): Unit = {
//    val model = IOHelper.deserializeModel(opts.modelFile.value, opts.lexiconUrl.value)
//    state.model = model
//  }
//
//  def printDoc(): Unit = {
//    val doc = state.docs(state.idx)
//    println(doc.name)
//    println(doc.tokens.size)
//    if (doc.attr[CitationSpanList] != null) {
//      println("Citation Span List:")
//      val csl = doc.attr[CitationSpanList]
//      for (c <- csl.spans) {
//        println(s"\t${c.toString()}")
//      }
//    }
//    println("\t\t*\t*\t*")
//    println("tokens:")
//    for (t <- doc.tokens) {
//      println(s"${t.string}\t${t.attr.toString()}")
//    }
//    println("\t\t*\t*\t*")
//    println("xml:")
//    println(doc.toXML)
//  }
//
//  def printTokens(): Unit = {
//    val doc = state.docs(state.idx)
//    for (t <- doc.tokens) {
//      val label = t.attr[CitationLabel].categoryValue
//      val gold = t.attr[CitationLabel].target.categoryValue
//      val mistake = if (label.equals(gold)) "" else "*"
//      println(s"$mistake\t${t.string}\t$label\t$gold")
//    }
//  }
//
//  def printSpans(): Unit = {
//    val doc = state.docs(state.idx)
//    val spans = doc.attr[CitationSpanBuffer]
//    for (s <- spans) {
//      println(s.toXML)
//    }
//  }
//
//  def printXML(): Unit = {
//    val doc = state.docs(state.idx)
//    println(doc.toXML)
//  }
//
//  def oversegment(): Unit = {
//    OverSegmenter.overSegment(state.docs, opts.lexiconUrl.value)
//  }
//}

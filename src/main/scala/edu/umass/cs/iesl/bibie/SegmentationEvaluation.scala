package edu.umass.cs.iesl.bibie

import cc.factorie.app.nlp.Document
import cc.factorie.variable.{CategoricalDomain, LabeledDiscreteEvaluation, LabeledMutableCategoricalVar}

import scala.reflect.ClassTag


class SegmentationEvaluation[A <: LabeledMutableCategoricalVar[String]](labelDomain: CategoricalDomain[String])(implicit m: ClassTag[A]) {

  def printEvaluation(trainDocuments: Iterable[Document], testDocuments: Iterable[Document], iteration: String): Double = {
    println("Iteration " + iteration)
    println("TRAIN")
    val trainF1 = evaluationString(trainDocuments).f1
    println("TEST")
    if(!testDocuments.isEmpty) {
      evaluationString(testDocuments).f1
    }
    else trainF1
  }

  def printEvaluationSingle(documents: Iterable[Document], extraText: String = ""): Double = {
    println(extraText)
    val es = evaluationString(documents)
    es.f1
  }

  def segmentationEvaluation(trainDocuments: Iterable[Document], testDocuments: Iterable[Document], iteration: String): Double = {
    println("Segmentation evaluation")
    println("TRAIN")
    segEvaluationString(trainDocuments)
    println("TEST")
    var es = segEvaluationString(testDocuments)
    println("Iteration " + iteration)
    es
  }

  def getDocsWithMistakes(testDocuments: Iterable[Document]): Iterable[Document] = testDocuments.filter(_.tokens.exists(!_.attr[A].valueIsTarget))
  def getDocTargetString(d: Document): String = d.name + "\n" + d.tokens.toSeq.map(t => t.string + "\t" + t.attr[A].target.categoryValue).mkString("\n")
  def getDocPredictedString(d: Document): String = d.name + "\n" + d.tokens.toSeq.map(t => t.string + "\t" + t.attr[A].categoryValue).mkString("\n")
  def getAllDocsTargetString(ds: Iterable[Document]): String = ds.map(getDocTargetString).mkString("\n\n")
  def getAllDocsPredictedString(ds: Iterable[Document]): String = ds.map(getDocPredictedString).mkString("\n\n")

  def writeMistakesAndTarget(ds: Iterable[Document], filePrefix: String): Unit = {
    val docs = ds.toSeq
    val predSuffix = "-predictions"
    val targetSuffix = "-gold"
    import scala.tools.nsc.io.File
    File(filePrefix + predSuffix).writeAll(getAllDocsPredictedString(docs))
    File(filePrefix + targetSuffix).writeAll(getAllDocsTargetString(docs))
  }


  def evaluationString(documents: Iterable[Document]): SegEvaluationReport = {
    //println("Train Token accuracy = "+ NerObjective.aveScore(trainLabels))
    //println(" Test Token accuracy = "+ NerObjective.aveScore(testLabels))
    val buf = new StringBuffer
    // Per-token evaluation
    buf.append(new LabeledDiscreteEvaluation(documents.flatMap(_.tokens.map(_.attr[A]))))
    val segmentEvaluation = new cc.factorie.app.chain.SegmentEvaluation[A](labelDomain.categories.filter(x => x.length > 2 && (x.startsWith("I") || x.startsWith("B"))).map(_.substring(2)), "(B|I|U)-", "(I|L)-")
    for (doc <- documents; sentence <- doc.sentences) segmentEvaluation += sentence.tokens.map(_.attr[A])
    println(segmentEvaluation)
//    println("Segment evaluation")
//    println(segmentEvaluation.f1)
//    //println(segmentEvaluation)
//    println("Token level evaluation")
//    println(segmentEvaluation.tokenAccuracy)
    SegEvaluationReport(segmentEvaluation.f1, segmentEvaluation.tokenAccuracy)
  }
  def segEvaluationString(documents: Iterable[Document]): Double = {
    var totalSegments = 0
    var incorrectSegments = 0
    for (d <- documents) {
      var t = d.tokens.head
      while (t != null) {
        val l = t.attr[A]
        if (l.categoryValue != "O" && l.target.categoryValue != "O") {
          val lpred = l.categoryValue.split(":").last
          val ltarg = l.target.categoryValue.split(":").last
          val lptag = lpred.drop(2)
          val lttag = ltarg.drop(2)
          if (ltarg.startsWith("B")) {
            totalSegments += 1
            if (!lpred.startsWith("B")) incorrectSegments += 1
            else {
              var search = t
              var good = true
              while (search.hasNext && search.next.attr[A].target.categoryValue.split(":").last.startsWith("I-" + lttag)) {
                search = search.next
                if (!search.attr[A].categoryValue.split(":").last.startsWith("I-" + lptag)) good = false
              }
              t = search
              if (!good) incorrectSegments += 1
            }
          }

        }
        t = if (t.hasNext) t.next else null
      }
    }
    println("Incorrect segments: " + incorrectSegments)
    println("Total segments " + totalSegments)
    println("Error: " + incorrectSegments.toDouble / totalSegments.toDouble)
    incorrectSegments.toDouble / totalSegments.toDouble
  }

}


case class SegEvaluationReport(f1: Double, tokenAccuracy: Double)
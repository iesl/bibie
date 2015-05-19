package edu.umass.cs.iesl.bibie

import cc.factorie.variable.{CategoricalDomain, LabeledMutableCategoricalVar}
import cc.factorie.app.nlp._

import scala.collection.mutable.{HashSet, HashMap, ArrayBuffer}
import scala.util.matching.Regex
import java.io._

/**
 * Created by kate on 5/14/15.
 */
class ExactlyLikeGrobidEvaluator(labelDomain: CategoricalDomain[String]) {
  def baseLabel(s: String): String = s.substring(2)
  def evaluate(docs: Seq[Document]): String = {
    val report = new StringBuilder()
    val labels = new ArrayBuffer[String]()
    val counterObserved = new ArrayBuffer[Int]()
    val counterExpected = new ArrayBuffer[Int]()
    val counterFalsePositive = new ArrayBuffer[Int]()
    val counterFalseNegative = new ArrayBuffer[Int]()
    var i = 0
    while (i < docs.length) {
      val doc = docs(i)
      val tokenSeq = doc.tokens.toIndexedSeq
      var currToken: String = null
      var prevToken: String = null
      var j = 0
      while (j < doc.tokens.size) {
        val token = tokenSeq(j)
        currToken = baseLabel(token.attr[CitationLabel].categoryValue)
        prevToken =
          if (token.attr.contains(classOf[GoldCitationLabel])) baseLabel(token.attr[GoldCitationLabel].label)
          else baseLabel(token.attr[CitationLabel].target.categoryValue)
        val ind = labels.indexOf(prevToken)
        if (ind != -1) {
          if (prevToken == currToken) {
            val count = counterObserved(ind)
            counterObserved.update(ind, count + 1)
          } else {
            val ind2 = labels.indexOf(currToken)
            if (ind2 != -1) {
              val count = counterFalsePositive(ind2)
              counterFalsePositive.update(ind2, count + 1)
            } else {
              labels += currToken
              counterFalsePositive += 1
              counterObserved += 0
              counterExpected += 0
              counterFalseNegative += 0
            }
            val count2 = counterFalseNegative(ind)
            counterFalseNegative.update(ind, count2 + 1)
          }
          val count = counterExpected(ind)
          counterExpected.update(ind, count + 1)
        } else {
          labels += prevToken
          if (prevToken == currToken) {
            counterObserved += 1
            counterFalsePositive += 0
            counterFalseNegative += 0
          } else {
            counterObserved += 0
            counterFalsePositive += 0
            counterFalseNegative += 1
            val ind2 = labels.indexOf(currToken)
            if (ind2 != -1) {
              val count = counterFalsePositive(ind2)
              counterFalsePositive.update(ind2, count + 1)
            } else {
              labels += currToken
              counterFalsePositive += 1
              counterObserved += 0
              counterExpected += 0
              counterFalseNegative += 0
            }
          }
          counterExpected += 1
        }
        j += 1
      }
      i += 1
    }

    report.append("\n===== Token-level results =====\n\n")
    val tokenLevelMetrics = computeMetrics(labels, counterObserved, counterExpected, counterFalsePositive, counterFalseNegative)
    report.append(tokenLevelMetrics)
    val tokenLvlFile = "/iesl/canvas/ksilvers/bibie-exec/tokenLevelMine.eval"
    println("writing token-level results to file: " + tokenLvlFile)
    val pw = new PrintWriter(new File(tokenLvlFile))
    pw.write(tokenLevelMetrics)
    pw.close()

    // TODO field-level, instance-level

    report.toString()
  }

  def computeMetrics(labels: ArrayBuffer[String],
                      counterObserved: ArrayBuffer[Int],
                      counterExpected: ArrayBuffer[Int],
                      counterFP: ArrayBuffer[Int],
                      counterFN: ArrayBuffer[Int]): String = {
    def fmt2(d: Double): String = "%.2f".format(d)
    val report = new StringBuilder()
    report.append("\nlabel\t\taccuracy\tprecision\trecall\t\tf1\n\n")
    class Accum(var tp: Int, var fp: Int, var tn: Int, var fn: Int, var all: Int) {
      var accuracy: Double = 0.0
      var f0: Double = 0.0
      var recall: Double = 0.0
      var precision: Double = 0.0
    }
    val accum = new Accum(0, 0, 0, 0, 0)
    var totalValidFields = 0
    var totalFields = 0
    var i = 0
    while (i < labels.size) {
      totalFields += counterExpected(i)
      i += 1
    }
    i = 0
    while (i < labels.size) {
      totalFields += counterFP(i)
      i += 1
    }
    var accuracy = 0.0
    var precision = 0.0
    var recall = 0.0
    var f0 = 0.0
    i = 0
    while (i < labels.size) {
      val label = labels(i).trim //FIXME why trim?
      if (label == "other" || label == "base") {
        i += 1
      } else {
        report.append("<" + label + ">")
        if (label.length < 12) report.append("\t")
        val tp = counterObserved(i)
        val fp = counterFP(i)
        val fn = counterFN(i)
        val tn = totalFields - tp - (fp + fn)
        val all = counterExpected(i)
        if (all != 0) totalValidFields += 1 //FIXME should this be != 0 or != -1?

        accuracy = 1.0 * (tp + tn) / (tp + fp + tn + fn)
        report.append("\t").append(fmt2(accuracy*100.0))

        precision = if ((tp + fp) == 0) 0.0 else 1.0 * tp / (tp + fp)
        report.append("\t\t").append(fmt2(precision * 100.0))

        recall = if (tp == 0 || all == 0) 0.0 else 1.0 * tp / all
        report.append("\t\t").append(fmt2(recall * 100.0))

        f0 = if ((precision + recall) == 0) 0.0 else (2.0 * precision * recall) / (precision + recall)
        report.append("\t\t").append(fmt2(f0 * 100.0))

        report.append("\n")

        accum.tp += tp
        accum.fp += fp
        accum.tn += tn
        accum.fn += fn
        if (all != 0) {
          accum.all += all
          accum.accuracy += accuracy
          accum.f0 += f0
          accum.precision += precision
          accum.recall += recall
        }
        i += 1
      }
    }
    report.append("\n")
    report.append("all fields (micro)\t")

    //micro average
    accuracy = 1.0 * (accum.tp + accum.tn) / (accum.tp + accum.fp + accum.tn + accum.fn)
    report.append("\t").append(fmt2(accuracy * 100.0))
    precision = 1.0 * accum.tp / (accum.tp + accum.fp)
    report.append("\t\t").append(fmt2(precision * 100.0))
    recall = 1.0 * accum.fp / accum.all
    report.append("\t\t").append(fmt2(recall * 100.0))
    f0 = (2.0 * precision * recall) / (precision + recall)
    report.append("\t\t").append(fmt2(f0 * 100.0))
//    report.append("\t(micro average)")
    report.append("\n")

    //macro average
//    report.append("\t\t")
    report.append("all fields (macro)\t")
    accuracy = accum.accuracy / totalValidFields
    report.append("\t").append(fmt2(accuracy * 100.0))
    precision = accum.precision / totalValidFields
    report.append("\t\t").append(fmt2(precision * 100.0))
    recall = accum.recall / totalValidFields
    report.append("\t\t").append(fmt2(recall * 100.0))
    f0 = accum.f0 / totalValidFields
    report.append("\t\t").append(fmt2(f0 * 100.0))
//    report.append("\t(macro average)")
    report.append("\n")

    report.toString()
  }

}


class FieldLevelEvaluator(labelDomain: CategoricalDomain[String]) {
  case class EvalField(label: String) {
    var trueNeg = 0; var truePos = 0; var falseNeg = 0; var falsePos = 0; var numExpected = 0
    def precision: Double = if (truePos + falsePos > 0) truePos.toDouble / (truePos + falsePos) else 0.0
    def recall: Double = if (truePos + falseNeg > 0) truePos.toDouble / (truePos + falseNeg) else 0.0
    def f1: Double = {
      val p = precision; val r = recall
      if (p + r > 0) (2.0*p*r) / (p+r) else 0.0
    }
    override def toString: String = "%-8s f1=%-6f p=%-6f r=%-6f (tp=%d fp=%d fn=%d true=%d pred=%d)".format(label, f1, precision, recall, truePos, falsePos, falseNeg, numExpected, truePos+falsePos)
  }
  def evaluate(docs: Seq[Document]): Double = {
    val fields = new HashMap[String, EvalField]()
    val baseCategories = labelDomain.categories.toSet
    println(s"labels: ${baseCategories.mkString(", ")}")
    baseCategories.foreach { c => fields(c) = new EvalField(c) }
    docs.foreach { doc =>
      val tokenseq = doc.tokens.toSeq
      var i = 0
//      var lastSeen = tokenseq.head
      while (i < tokenseq.length) {
        val token = tokenseq(i)
        val guess = token.attr[CitationLabel].categoryValue
        val gold = token.attr[CitationLabel].target.categoryValue
        fields(gold).numExpected += 1
        if (guess == gold) fields(gold).truePos += 1
        else if (guess != gold) {
          fields(guess).falsePos += 1
          fields(gold).falseNeg += 1
        }
        i += 1
      }
    }
    var avg = 0.0
    fields.foreach { case (label, eval) =>
        println(eval.toString)
        avg += eval.f1
    }
    val overall = avg / fields.size.toDouble
    println(s"OVERALL: $overall")
    overall
  }
}


/** Evaluate in terms of correct entire segments.
    The field start and end boundaries must be perfect to count as correct.  No partial credit.
    For example, this is the standard for results on CoNLL 2003. */
class MyPerSegmentEvaluation(val labelName:String, val labelValueStart: Regex, val labelValueContinue: Regex) {
  //println("PerSegmentEvaluation "); println(labelName); println(labelValueStart); println(labelValueContinue); println
  //if (labelValueContinue == null) labelValueContinue = labelValueStart // Waiting for Scala 2.8 default parameters

  var targetCount, predictedCount, correctCount = 0 // per segment

  def ++=(tokenseqs:Seq[IndexedSeq[{def label:LabeledMutableCategoricalVar[String]}]]): Unit = tokenseqs.foreach(ts => this.+=(ts.map(_.label)))  // TODO this triggers reflection

  /* Find out if we are at the beginning of a segment.
   * This complicated conditional is necessary to make the start pattern "(B|I)-" work
   * for both BIO and IOB formats. We are at a start if either (a) only labelValueStart
   * matches, or (b) labelValueContinue matches and the previous label doesn't match
   * The (b) case makes it work for IOB notation, in which "B-*" is only used at the
   * boundary between two like-categoried mentions. */
  protected def isSegmentStart(x:String, prev:String) =
    (isStart(x) && !isContinue(x)) || (isContinue(x) && (prev == null || isBackground(prev)))
  protected def isBackground(x:String) = !isStart(x) && !isContinue(x)
  protected def isStart(x:String) = labelValueStart.pattern.matcher(x).matches
  protected def isContinue(x:String) = labelValueContinue.pattern.matcher(x).matches

  /** Add the given sequence of labels to the statistics for this evalution.

      Note: Putting all Label instances across all sentences in a single Seq[]
      may result in slightly incorrect results at document boundaries: when one
      document ends in a mention and the next document begins with the same
      mention type, they will be counted as only one mention, when they should
      have been counted as two. */
  def +=(labels: IndexedSeq[LabeledMutableCategoricalVar[String]]): Unit = {
    //println("PerSegmentEvaluation += "+labels.size)
    var predictedStart, targetStart = false
    for (position <- 0 until labels.length) {
      val label = labels(position)
      val labelPrevValue: String = if (position > 0) labels(position - 1).categoryValue else null
      val labelPrevTargetValue: String = if (position > 0) labels(position - 1).target.categoryValue else null

      //print("\n"+label.token.word+"="+label.trueValue+"/"+label.value+" ")
      predictedStart = false; targetStart = false

      // Find out if we are at the beginning of a segment.
      if (isSegmentStart(label.categoryValue, labelPrevValue)) {
        predictedCount += 1
        predictedStart = true
        //print("ps ")
      }

      if (isSegmentStart(label.target.categoryValue, labelPrevTargetValue)) {
        targetCount += 1
        targetStart = true
        //print("ts ")
      }

      // Truth and prediction both agree that a segment is starting here, let's see if they end in the same place
      if (predictedStart && targetStart) {
        if(position == labels.length-1)
          correctCount += 1 // Both sequences ended at the same position: correct
        else { //Otherwise lets be sure they end at the same place
        var predictedContinue, targetContinue = false
          var j = position + 1
          var stopSearchForSegmentEnd = false
          while (j < labels.length && !stopSearchForSegmentEnd) {
            val label2 = labels(j)
            predictedContinue = isContinue(label2.categoryValue)
            targetContinue = isContinue(label2.target.categoryValue)
            // if true or predicted segment ends (i.e. is not a continue) or we reach the end of our label sequence.
            if (!predictedContinue || !targetContinue || j == labels.length - 1) {
              if (predictedContinue == targetContinue) {
                correctCount += 1 // Both sequences ended at the same position: correct
              }
              stopSearchForSegmentEnd = true
            }
            j += 1
          }
        }
      }
    }
  }
  def precision = if (predictedCount == 0) 1.0 else correctCount.toDouble / predictedCount
  def recall = if (targetCount == 0) 1.0 else correctCount.toDouble / targetCount
  def f1 = if (recall+precision == 0.0) 0.0 else (2.0 * recall * precision) / (recall + precision)
  def alarmCount = predictedCount - correctCount
  def missCount = targetCount - correctCount
  def tp = correctCount
  def fn = missCount
  def fp = alarmCount
  override def toString = "%-8s f1=%-6f p=%-6f r=%-6f (tp=%d fp=%d fn=%d true=%d pred=%d)".format(labelName, f1, precision, recall, tp, fp, fn, targetCount, predictedCount)
}


// Some utilities for automatically filling in values
object MySegmentEvaluation {
  // Assume that the first two characters of each label are the "B-" or "I-" prefix.  Skip the label "O" because it is less than 3 chars long
  def labelStringsToBase(labelVals:Seq[String]): Seq[String] = {
    val result = new HashSet[String]
    labelVals.foreach(s => if (s.length > 2) result += s)//.substring(2))
    result.toSeq
  }
}
// For defaultStartPrefix = "(B|I)-" Although just "B-" would be enough for BIO, "(B|I)-" is needed for IOB
// For BILOU you should use startPrefix = "(B|U)-" and continuePrefix = "(I|L)-"
class MySegmentEvaluation[L<:LabeledMutableCategoricalVar[String]](baseLabelStrings: Seq[String], startPrefix:String = "(B|I)-", continuePrefix:String = "I-") {
  def this(startPrefix:String, continuePrefix:String, labelDomain:CategoricalDomain[String]) = this(MySegmentEvaluation.labelStringsToBase(labelDomain.map(_.category)), startPrefix, continuePrefix)
  def this(startPrefix:String, continuePrefix:String, labelDomain:CategoricalDomain[String], labels:IndexedSeq[L]) = {
    this(MySegmentEvaluation.labelStringsToBase(labelDomain.map(_.category)), startPrefix, continuePrefix)
    this += labels
  }
  def this(labelDomain:CategoricalDomain[String]) = this(MySegmentEvaluation.labelStringsToBase(labelDomain.map(_.category)))
  // Grab the domain from the first label in the Seq; assume all the domains are the same
  def this(labels:IndexedSeq[L]) = { this(labels.head.domain); this.+=(labels) }
  private val evals = new HashMap[String,MyPerSegmentEvaluation]
  private var labelCount = 0
  private var labelCorrectCount = 0
  evals ++= baseLabelStrings.map(s => (s, new MyPerSegmentEvaluation(s, (startPrefix+s).r, (continuePrefix+s).r)))
  /** Return the LabelEvaluation specific to labelString. */
  def apply(labelString:String) = evals(labelString)
  def +=(labels: IndexedSeq[L]): Unit = {
    evals.values.foreach(eval => eval += labels)
    labelCount += labels.length
    labels.foreach(label => if (label.valueIsTarget) labelCorrectCount += 1)
  }

  // This is a per-label measure
  def tokenAccuracy = labelCorrectCount.toDouble / labelCount
  // The rest are per-segment
  def correctCount = evals.values.foldLeft(0)(_+_.correctCount)
  def predictedCount = evals.values.foldLeft(0)(_+_.predictedCount)

  def targetCount = evals.values.foldLeft(0)(_+_.targetCount)
  def precision = if (predictedCount == 0) 1.0 else correctCount.toDouble / predictedCount
  def recall = if (targetCount == 0) 1.0 else correctCount.toDouble / targetCount
  def f1: Double = if (precision + recall == 0.0) 0.0 else 2.0 * precision * recall / (precision + recall)
  def alarmCount = predictedCount - correctCount
  def missCount = targetCount - correctCount
  def tp = correctCount
  def fn = missCount
  def fp = alarmCount
  def summaryString = "%-8s f1=%-6f p=%-6f r=%-6f (tp=%d fp=%d fn=%d true=%d pred=%d) acc=%-5f (%d/%d)\n".format("OVERALL", f1, precision, recall, tp, fp, fn, tp+fn, tp+fp, tokenAccuracy, labelCorrectCount, labelCount)
  override def toString = {
    val sb = new StringBuffer
    //sb.append("ACCURACY "+tokenAccuracy+" ("+labelCorrectCount+"/"+labelCount+")")
    //sb.append("\n")
    sb.append(summaryString)
    evals.keys.toList.sortWith(_<_).foreach(l => {
      sb.append(evals(l).toString)
      sb.append("\n")
    })
    sb.toString
  }
}
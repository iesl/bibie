package edu.umass.cs.iesl.bibie

import cc.factorie.variable.{CategoricalDomain, LabeledMutableCategoricalVar}
import cc.factorie.app.nlp._

import scala.collection.mutable.{HashSet, HashMap, ArrayBuffer}
import scala.util.matching.Regex
import java.io._
import java.util.StringTokenizer
import scala.io.Source

/**
 * Created by kate on 5/14/15.
 */

class ExactlyLikeGrobidEvaluator {
  def grobidLabel(c: String): String =
    if (c.startsWith("B-")) "I-" + c.substring(2)
    else if (c.startsWith("I-")) c.substring(2)
    else c
  def evaluate(docs: Seq[Document], filename: String, withBIO: Boolean): String = {
    writeFile(docs, filename)
    evaluate(filename, withBIO=withBIO)
  }
  def evaluate(filename: String, withBIO: Boolean): String = {
    val report = new StringBuilder()
    val theResult = loadFile(filename)
    report.append(evaluateTokenLevel(theResult, withBIO))
    report.append(evaluateFieldLevel(theResult, withBIO))
    report.toString()
  }

  def evaluateFieldLevel(theResult: String, withBIO: Boolean): String = {
    val report = new StringBuilder()
    val labels = new ArrayBuffer[String]()
    val counterObserved = new ArrayBuffer[Int]()
    val counterExpected = new ArrayBuffer[Int]()
    val counterFP = new ArrayBuffer[Int]()
    val counterFN = new ArrayBuffer[Int]()

    var allGood: Boolean = true
    var lastPrevToken: String = null
    var lastCurrToken: String = null
    var line: String = null
    val stt = new StringTokenizer(theResult, "\n")
    while (stt.hasMoreTokens) {
      line = stt.nextToken()
      if ((line.trim.length == 0) && (lastPrevToken != null) && (lastCurrToken != null)) {
        var index = labels.indexOf(lastPrevToken)
        if (index == -1) {
          labels += lastPrevToken
          counterObserved += 0
          counterExpected += 0
          counterFP += 0
          counterFN += 0
          index = labels.indexOf(lastPrevToken)
        }
        if (allGood) {
          val count = counterObserved(index)
          counterObserved.update(index, count+1)
        } else {
          val count = counterFN(index)
          counterFN.update(index, count+1)
        }
        val count = counterExpected(index)
        counterExpected.update(index, count+1)

        index = labels.indexOf(lastCurrToken)
        if (index == -1) {
          labels += lastCurrToken
          counterObserved += 0
          counterExpected += 0
          counterFP += 0
          counterFN += 0
          index = labels.indexOf(lastCurrToken)
        }
        if (!allGood) {
          val v = counterFP(index)
          counterFP.update(index, v+1)
        }
        allGood = true
        lastPrevToken = null
        lastCurrToken = null
        //continue
      } else {
        val st = new StringTokenizer(line, "\t ")
        var currToken: String = null
        var prevToken: String = null
        while (st.hasMoreTokens) {
          currToken = st.nextToken()
          if (currToken != null) {
            if (currToken.startsWith("I-") || currToken.startsWith("E-")) {
              currToken = currToken.substring(2, currToken.length)
            }
          }
          if (st.hasMoreTokens) {
            prevToken = currToken
          }
        }
        if ((prevToken == null) || (currToken == null)) {
          lastPrevToken = null
          lastCurrToken = null
          //continue
        } else {

          if ((lastPrevToken != null) && (!prevToken.equals(lastPrevToken))) {
            if (!labels.contains(lastPrevToken)) {
              labels += (lastPrevToken)
              counterObserved += 0
              counterExpected += 0
              counterFP += 0
              counterFN += 0
            }
            val index = labels.indexOf(lastPrevToken)
            if (allGood) {
              val v = counterObserved(index)
              counterObserved.update(index, v+1)
            } else {
              val v = counterFN(index)
              counterFN.update(index, v+1)
            }
            val v = counterExpected(index)
            counterExpected.update(index, v+1)
          }

          if ((lastCurrToken != null) && (!currToken.equals(lastCurrToken))) {
            if (!labels.contains(lastCurrToken)) {
              labels += lastCurrToken
              counterObserved += 0
              counterExpected += 0
              counterFP += 0
              counterFN += 0
            }
            val index = labels.indexOf(lastCurrToken)
            if (!allGood) {
              val v = counterFP(index)
              counterFP.update(index, v + 1)
            }
          }

          if (((lastPrevToken != null) && (!(prevToken == lastPrevToken))) || ((lastCurrToken != null) && (!(currToken == lastCurrToken)))) {
            allGood = true
          }

          if (!currToken.equals(prevToken)) allGood = false

          lastPrevToken = prevToken
          lastCurrToken = currToken

        }
      }
    }

    if ((lastPrevToken != null) && (lastCurrToken != null)) {
      var index = labels.indexOf(lastPrevToken)
      if (index == -1) {
        labels += lastPrevToken
        counterObserved += 0
        counterExpected += 0
        counterFP += 0
        counterFN += 0
        index = labels.indexOf(lastPrevToken)
      }
      if (allGood) {
        val v = counterObserved(index)
        counterObserved.update(index, v+1)
      } else {
        val v = counterFN(index)
        counterFN.update(index, v+1)
      }
      val v = counterExpected(index)
      counterExpected.update(index, v+1)

      index = labels.indexOf(lastCurrToken)
      if (index == -1) {
        labels += lastCurrToken
        counterObserved += 0
        counterExpected += 0
        counterFP += 0
        counterFN += 0
        index = labels.indexOf(lastCurrToken)
      }
      if (!allGood) {
        val v = counterFP(index)
        counterFP.update(index, v+1)
      }
    }

    report.append("\n===== Field-level results =====\n")
    val fieldLevelMetrics = computeMetrics(labels.toList, counterObserved.toList, counterExpected.toList, counterFP.toList, counterFN.toList)
    report.append(fieldLevelMetrics)
    report.toString()
  }

  def evaluateTokenLevel(theResult: String, withBIO: Boolean): String = {
    val report = new StringBuilder()
    val labels = new ArrayBuffer[String]()
    val counterObserved = new ArrayBuffer[Int]()
    val counterExpected = new ArrayBuffer[Int]()
    val counterFP = new ArrayBuffer[Int]()
    val counterFN = new ArrayBuffer[Int]()

    var line: String = null
    //    val theResult = loadFile(filename)

    val stt = new StringTokenizer(theResult, "\n")
    while (stt.hasMoreTokens) {
      line = stt.nextToken()
      if (line.trim.length != 0) {
        val st = new StringTokenizer(line, "\t ")
        var currToken: String = null
        var prevToken: String = null
        while (st.hasMoreTokens) {
          currToken = st.nextToken()
          if (currToken != null) {
            if (currToken.startsWith("I-") || currToken.startsWith("E-")) {
              currToken = currToken.substring(2, currToken.length)
            }
          }
          if (st.hasMoreTokens) prevToken = currToken
        }

        if ((prevToken != null) && (currToken != null)) {
          val ind = labels.indexOf(prevToken)
          if (ind != -1) {
            if (prevToken == currToken) {
              val count = counterObserved(ind)
              counterObserved.update(ind, count+1)
            } else {
              val ind2 = labels.indexOf(currToken)
              if (ind2 != -1) {
                val count = counterFP(ind2)
                counterFP.update(ind2, count+1)
              } else {
                labels += currToken
                counterFP += 1
                counterObserved += 0
                counterExpected += 0
                counterFN += 0
              }
              val count2 = counterFN(ind)
              counterFN.update(ind, count2+1)
            }
            val count = counterExpected(ind)
            counterExpected.update(ind, count+1)
          } else {
            labels += prevToken
            if (prevToken == currToken) {
              counterObserved += 1
              counterFP += 0
              counterFN += 0
            } else {
              counterObserved += 0
              counterFP += 0
              counterFN += 1
              val ind2 = labels.indexOf(currToken)
              if (ind2 != -1) {
                val count = counterFP(ind2)
                counterFP.update(ind2, count+1)
              } else {
                labels += currToken
                counterFP += 1
                counterObserved += 0
                counterExpected += 0
                counterFN += 0
              }
            }
            counterExpected += 1
          }
        }
      }
    } // while (stt.hasMoreTokens)

    report.append("\n===== Token-level results =====\n\n")
    val tokenLevelMetrics = computeMetrics(labels.toList, counterObserved.toList, counterExpected.toList, counterFP.toList, counterFN.toList)
    report.append(tokenLevelMetrics)
    report.toString()
  }

  def computeMetrics(labels: List[String],
                     counterObserved: List[Int],
                     counterExpected: List[Int],
                     counterFP: List[Int],
                     counterFN: List[Int]): String = {
    def fmt2(d: Double): String = "%.2f".format(d)
    val report = new StringBuilder()
    report.append("\nlabel\t\taccuracy\tprecision\trecall\t\tf1\n\n")

    var cumulated_tp = 0
    var cumulated_fp = 0
    var cumulated_tn = 0
    var cumulated_fn = 0
    var cumulated_f0 = 0.0
    var cumulated_accuracy = 0.0
    var cumulated_precision = 0.0
    var cumulated_recall = 0.0
    var cumulated_all = 0
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
      val label = labels(i).trim
      if (label.equals("<other>") || label.equals("base")) {
        i += 1
        //continue
      } else {
        report.append(label)
        if (label.length < 12) {
          report.append("\t")
        }
        var tp = counterObserved(i) // true positives
        var fp = counterFP(i) // false positives
        var fn = counterFN(i) // false negative
        var tn = totalFields - tp - (fp + fn) // true negatives
        var all = counterExpected(i) // all expected

        if (all != 0) {
          totalValidFields += 1
        }

        accuracy = 1.0 * (tp + tn) / (tp + fp + tn + fn)
        report.append("\t").append(fmt2(accuracy * 100))

        // report.append("\t"+ "-")

        precision = 0.0
        if ((tp + fp) == 0) {
          precision = 0.0
        } else {
          precision = 1.0 * (tp) / (tp + fp)
        }
        report.append("\t\t").append(fmt2(precision * 100))

        recall = 0.0
        if ((tp == 0) || (all == 0)) {
          recall = 0.0
        } else {
          recall = 1.0 * (tp) / all
        }
        report.append("\t\t").append(fmt2(recall * 100))

        f0 = 0.0
        if (precision + recall == 0) {
          f0 = 0.0
        } else {
          f0 = (2 * precision * recall) / (precision + recall)
        }
        report.append("\t\t").append(fmt2(f0 * 100))

        report.append("\n")

        cumulated_tp += tp
        cumulated_fp += fp
        cumulated_tn += tn
        cumulated_fn += fn
        if (all != 0) {
          cumulated_all += all
          cumulated_f0 += f0
          cumulated_accuracy += accuracy
          cumulated_precision += precision
          cumulated_recall += recall
        }
        i += 1
      }
    }

    report.append("\n")
    report.append("all fields (micro)\t")

    // micro average over measures
    accuracy = 1.0 * (cumulated_tp + cumulated_tn) / (cumulated_tp + cumulated_fp + cumulated_tn + cumulated_fn)
    if (accuracy > 1)
      accuracy = 1.0
    report.append("\t").append(fmt2(accuracy * 100))

    precision = 1.0 * cumulated_tp / (cumulated_tp + cumulated_fp)
    if (precision > 1)
      precision = 1.0
    report.append("\t\t").append(fmt2(precision * 100))

    //recall = (1.0 * cumulated_tp) / (cumulated_tp + cumulated_fn)
    recall = (1.0 * cumulated_tp) / (cumulated_all)
    if (recall > 1)
      recall = 1.0
    report.append("\t\t").append(fmt2(recall * 100)).append(" ")

    f0 = (2 * precision * recall) / (precision + recall)
    report.append("\t\t").append(fmt2(f0 * 100))
    //		report.append("\t(micro average)")
    report.append("\n")

    report.append("all fields (macro)\t")
    // macro average over measures
    //		report.append("\t\t")
    accuracy = cumulated_accuracy / (totalValidFields)
    if (accuracy > 1)
      accuracy = 1.0
    report.append("\t").append(fmt2(accuracy * 100))

    precision = cumulated_precision / totalValidFields
    if (precision > 1)
      precision = 1.0
    report.append("\t\t").append(fmt2(precision * 100))

    recall = cumulated_recall / totalValidFields
    if (recall > 1)
      recall = 1.0
    report.append("\t\t").append(fmt2(recall * 100))

    f0 = cumulated_f0 / totalValidFields
    report.append("\t\t").append(fmt2(f0 * 100))

    //		report.append("\t(macro average)")
    report.append("\n")

    report.toString()
  }
  def loadFile(filename: String): String = {
    var lineCount = 0
    var line: String = null
    val okayLines = new ArrayBuffer[String]()
    try {
      val buffReader = new BufferedReader(new InputStreamReader(new FileInputStream(filename), "UTF-8"))
      var theResult: String = null
      line = buffReader.readLine()
      while (line != null) {
        okayLines += line
        lineCount += 1
        line = buffReader.readLine()
      }
      buffReader.close()
    } catch {
      case e: Exception => println(e)
    }
    println("got lineCount = " + lineCount)
    okayLines.mkString("\n")
  }
  def writeFile(docs: Seq[Document], filename: String): Unit = {
    println(s"writing output to: $filename ...")
    val pw = new PrintWriter(new File(filename))
    docs.foreach { doc =>
      doc.tokens.foreach { token =>
        val feats = List(token.string, 0).mkString("\t")
        val gold = grobidLabel(
          if (token.attr.contains(classOf[GoldCitationLabel])) token.attr[GoldCitationLabel].label
          else token.attr[CitationLabel].target.categoryValue
        )
        val guess = grobidLabel(token.attr[CitationLabel].categoryValue)
        val str = List(feats, gold, guess).mkString("\t")
        pw.write(str + "\n")
      }
      pw.write("\n")
    }
    pw.close()
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
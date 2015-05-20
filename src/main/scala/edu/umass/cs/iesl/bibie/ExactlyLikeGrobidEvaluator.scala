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

/**
 * TODO remove <other> field from evals / output
 * TODO instance-level evaluation
 * TODO verify ELGEval == GROBID Eval
 */

/** Close to verbatim copy of GROBID evaluation (in grobid-trainer, EvaluationUtilities.evaluateStandard) **/
class ExactlyLikeGrobidEvaluator {

  def evaluate(docs: Seq[Document], filename: String): String = {
    writeFile(docs, filename)
    evaluate(filename)
  }

  def evaluate(filename: String): String = {
    val report = new StringBuilder()
    val theResult = loadFile(filename)
    report.append(evaluateTokenLevel(theResult))
    report.append(evaluateFieldLevel(theResult))
    report.toString()
  }

  def evaluateFieldLevel(theResult: String): String = {
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

  def evaluateTokenLevel(theResult: String): String = {
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

  def evaluateInstanceLevel(theResult: String): String = {
    val report = new StringBuilder()
    val result = theResult.replace("\n\n", "\n \n")

    report.toString()

  }

  /** compute precision, recall, and f1 scores (both micro and macro averaged). Variables named "f0" should probably
    * be changed to "f1" **/
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

  /**
   * Data in [filename] should be tab-separated and structured as follows:
   *
   * feature1 feature2  ... feature_d gold_label  predicted_label
   *
   * where each line corresponds to a token. [filename] should contain all documents. Documents should be separated by
   * newlines.
   */
  def loadFile(filename: String): String = {
    var lineCount = 0
    var line: String = null
    val okayLines = new ArrayBuffer[String]()
    try {
      val buffReader = new BufferedReader(new InputStreamReader(new FileInputStream(filename), "UTF-8"))
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
//    println("got lineCount = " + lineCount)
    okayLines.mkString("\n")
  }

  /** write [docs] to [filename] in the format described in the comment on loadFile **/
  def writeFile(docs: Seq[Document], filename: String): Unit = {
    def grobidLabel(c: String): String =
      if (c.startsWith("B-")) "I-" + c.substring(2)
      else if (c.startsWith("I-")) c.substring(2)
      else c
    println(s"writing output to: $filename ...")
    val pw = new PrintWriter(new File(filename))
    docs.foreach { doc =>
      doc.tokens.foreach { token =>
        val feats = List(token.string, 0).mkString("\t")
        /* here, token will have attr GoldCitationLabel if it was loaded from a file produced by GROBID directly
        (see LoadGrobid.loadFromFilenameLabeled) */
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

/** Data structure for comparison/analysis of evaluations produced by GROBID and ExactlyLikeGrobidEvaluator **/
class EvalLine(val label: String, val accuracy: Double, val precision: Double, val recall: Double, val f0: Double) {
  def equals(other: EvalLine): Boolean = label == other.label && accuracy == other.accuracy && precision == other.precision && recall == other.recall && f0 == other.f0
  def diff(other: EvalLine): EvalLine = {
    assert(label == other.label, s"labels dont match: this=$label , other=${other.label}")
    new EvalLine(label, accuracy-other.accuracy, precision-other.precision, recall-other.recall, f0-other.f0)
  }
  override def toString: String = s"$label\t\t$accuracy\t\t$precision\t\t$recall\t\t$f0"
}
object EvalLine {
  val whitespace = "(\\s\\s)+".r
  def apply(s: String): EvalLine = {
    val parts = whitespace.split(s).filter(_.length > 0)
    assert(parts.length >= 5, s"bad line? $s --> [${parts.mkString(",")}] (${parts.length})")
    //    println("parts: " + parts.mkString(", "))
    val Array(label, accuracy, precision, recall, f0) = parts.take(5)
    val labelNorm = if (!label.startsWith("<")) "<" + label + ">" else label
    new EvalLine(labelNorm, accuracy.toDouble, precision.toDouble, recall.toDouble, f0.toDouble)
  }
}
/** Data structure for comparison/analysis of evaluations produced by GROBID and ExactlyLikeGrobidEvaluator **/
class Eval(val lines: Seq[EvalLine]) {
  def equals(other: Eval): Boolean =
    (lines.length == other.lines.length) &&
      lines.zip(other.lines).forall { case (l1, l2) => l1.equals(l2) }
  def diff(other: Eval): Eval = {
    assert(lines.length == other.lines.length, "eval lengths dont match")
    val l1Sorted = lines.sortBy { case l => l.label }
    val l2Sorted = other.lines.sortBy { case l => l.label }
    val diffs = l1Sorted.zip(l2Sorted).map { case (l1, l2) => l1.diff(l2) }
    new Eval(diffs)
  }
  def labels: Set[String] = lines.map(_.label).toSet
  override def toString: String = lines.map(_.toString).mkString("\n")
}

object Eval {
  def apply(filename: String): Eval = {
    println(s"loading Eval from $filename ...")
    val lines = scala.io.Source.fromFile(filename).getLines().toSeq
    val evalLines = lines.filter(_.length > 0).drop(1).map(l => EvalLine(l))
    new Eval(evalLines)
  }
}

/** Compare two Eval's **/
object Comparison {
  def main(args: Array[String]): Unit = {
//    println(args)

    val f1 = args(0)
    val f2 = args(1)

    val eval1 = Eval(args(0))
    val eval2 = Eval(args(1))
    println(eval1.equals(eval2))

//    println(s"$f1: labels = ${eval1.labels.mkString(", ")}")
//    println(s"$f2: labels = ${eval2.labels.mkString(", ")}")

    println(s"=== $f1 (eval1) ===")
    println(eval1.toString)
    println("")

    println(s"=== $f2 (eval2) ===")
    println(eval2.toString)
    println("")

    println(s"=== DIFF ($f1 - $f1) ===")
    val diff = eval1.diff(eval2)
    println(diff.toString)


  }
}


package edu.umass.cs.iesl.bibie

import cc.factorie._
import cc.factorie.la._
import cc.factorie.app.nlp.Token
import infer._
import scala.collection.mutable.ArrayBuffer
import cc.factorie.variable.{MutableDiscreteVar, CategoricalDomain, DiscreteVar}
import cc.factorie.model.Weights2
import cc.factorie.app.chain.{ChainModel, ChainHelper, ChainCliqueValues}

trait Results {
  def setToMax(variables: Seq[MutableDiscreteVar]): Unit = nodeMarginals.zip(variables).map({case (nm, v) => v.set(nm.maxIndex)(null)})
  def nodeMarginals: Array[DenseTensor1]
  def logZ: Double
}

case class ViterbiResults(mapScore: Double, mapValues: Array[Int], localScores: Seq[DenseTensor1]) extends Results {
  val nodeMarginals: Array[DenseTensor1] = {
    val domainSize = localScores(0).size
    mapValues.map(i => {val t = new DenseTensor1(domainSize); t(i) = 1.0; t})
  }
  def logZ: Double = mapScore
}
//
//case class InferenceResults(logZ: Double, nodeMarginals: Array[DenseTensor1], cliqueMarginals: Array[DenseTensor2]) extends Results
//
//object InferenceHelper {
//  def inferFast(scores: ChainCliqueValues, inverseTemperature: Double = 1.0): InferenceResults = {
//    if (inverseTemperature != 1.0)
//      scores *= inverseTemperature
//    val fbRes = ChainHelper.inferFast(scores)
//    val cm = ChainHelper.calculateCliqueMarginals(fbRes)
//    val logZ = fbRes.logZ / inverseTemperature
//    val res = InferenceResults(logZ, cm.localValues.toArray, cm.transitionValues.map(_.asInstanceOf[DenseTensor2]).toArray)
//    if (inverseTemperature != 1.0)
//      scores *= (1.0 / inverseTemperature)
//    res
//  }
//}

object BIOHelper {
  // constrains transitions to only allow valid BIO chunks
  // FIXME this allows citations to start with I's - fixed in the citation-specific local scores right now -luke
  def constrainTransitions(transitionWeights: Tensor2, labelDomain: CategoricalDomain[String]): Unit = {
    for (i <- labelDomain; j <- labelDomain)
      if (i.category.length > 1 && j.category.length > 1) {
        val is = i.category.split(":")
        val js = j.category.split(":")
        if (!js.filter(_.startsWith("I-")).forall(j => is.exists(i => i.substring(2) == j.substring(2))))
          transitionWeights(i.intValue, j.intValue) = Double.NegativeInfinity
      }
    for (i <- labelDomain; j <- labelDomain) {
      //      labelDomain.foreach(println)
      if (i.category == "" || i.category == "O") {
        val js = j.category.split(":")
        if (js.exists(_.startsWith("I-")))
          transitionWeights(i.intValue, j.intValue) = Double.NegativeInfinity
      }
    }
  }
  def getConstrainedTransitionTensor(labelDomain: CategoricalDomain[String]): Tensor2 = {
    val t = new DenseTensor2(labelDomain.dimensionSize, labelDomain.dimensionSize)
    constrainTransitions(t, labelDomain)
    t
  }
}

object CitationBIOHelper {
  lazy val allowedCitationTags: Seq[(Token => Boolean, List[String])] = {
    val map = ArrayBuffer[(Token => Boolean, List[String])]()
    map += ((a: Token) => a.string.toLowerCase.matches("tech|pre-print|preprint"), List("tech"))
    map += ((a: Token) => a.string == "Journal", List("journal"))
    map += ((a: Token) => a.string == "Russian" || a.string == "translated", List("language"))
    map += ((a: Token) => a.string == "Reprint", List("edition"))
    map += ((a: Token) => a.string.toLowerCase == "erratum", List("", "title"))
    map += ((a: Token) => a.string.toLowerCase == "chapter", List("chapter"))
    map += ((a: Token) => a.string.toLowerCase.contains("www."), List("venue:B-web"))
    map += ((a: Token) => a.string.toLowerCase.contains("http"), List("venue:B-web"))
    map += ((a: Token) => a.string == "Vol", List("volume"))
    map += ((a: Token) => a.string.matches(".*\\d.*") && a.hasPrev && a.prev.string.toLowerCase.startsWith("vol"), List("volume"))
    map += ((a: Token) => a.string.matches(".*\\d.*") && a.hasPrev && a.prev.string.toLowerCase == "no", List("number"))
    map += ((a: Token) => a.string.matches(".*\\d.*") && a.hasPrev && a.prev.string.length == 1 && a.prev.hasPrev && a.prev.prev.string.toLowerCase == "no", List("number"))
    map += ((a: Token) => a.string.toLowerCase.startsWith("thesis"), List("thesis"))
    map += ((a: Token) => a.string.toLowerCase.startsWith("dissertation"), List("thesis"))
    map += ((a: Token) => a.string.startsWith("Symp"), List("booktitle"))
    map += ((a: Token) => a.string.toLowerCase.startsWith("proc."), List("booktitle"))
    map += ((a: Token) => a.string.toLowerCase.startsWith("proceed"), List("booktitle"))
    map += ((a: Token) => a.hasPrev && a.prev.string == "In", List("editor", "booktitle", "language", "", "status", "series"))
    map += ((a: Token) => a.hasPrev && a.prev.string == "in" && a.prev.hasPrev && a.prev.prev.string.length == 1, List("series", "editor", "booktitle", "language", "", "status"))
    map += ((a: Token) => a.hasPrev && a.prev.string.length == 1 && a.prev.hasPrev && a.prev.prev.string == "In", List("series", "editor", "booktitle", "language"))
    map += ((a: Token) => a.string.toLowerCase.contains("submitted") || a.string.toLowerCase.contains("preparation"), List("status"))
    map
  }
  lazy val constrainedTransitionTensor = BIOHelper.getConstrainedTransitionTensor(LabelDomain)
  def constrainLocal(varying: Seq[DiscreteVar], localScores: Seq[DenseTensor1], labelDomain: CategoricalDomain[String]) = {
    val map = allowedCitationTags
    val d1 = labelDomain.dimensionSize
    for (i <- 0 until localScores.size) {
      var valid: List[String] = null
      for (a <- map) if (a._1(varying(i).asInstanceOf[CitationLabel].token)) valid = a._2
      for (vi <- 0 until d1) {
        val ls = localScores(i)
        if (valid != null)
          if (!valid.exists(v => LabelDomain(vi).category.contains(v))) ls(vi) = Double.NegativeInfinity
      }
    }
  }
  def getLocalScores(varying: Seq[DiscreteVar], theModel: CitationCRFModel): Array[DenseTensor1] = {
    val obsWeights = theModel.localTemplate.weights.value
    val a = Array.fill[DenseTensor1](varying.size)(null)
    var i = 0
    while (i < varying.length) {
      val scores = (obsWeights * varying(i).asInstanceOf[CitationLabel].token.attr[CitationFeatures].value).asInstanceOf[DenseTensor1]
      a(i) = scores
      i += 1
    }

    // constrain to not allow starting with anything containing I
    val badIndices = LabelDomain.toVector.filter(_.category.contains("I-")).map(cv => LabelDomain.index(cv.category))
    for (bi <- badIndices) a(0)(bi) = Double.NegativeInfinity

    a
  }
  def getScores(variables: Seq[DiscreteVar], model: Model, constrained: Boolean = true): ChainCliqueValues = {
    val m = model.asInstanceOf[CitationCRFModel]
    val markovScoresT = m.transitionTemplate.weights.value + constrainedTransitionTensor
    val markovScores = (0 until variables.size - 1).map(_ => markovScoresT.copy)
    val localScores = getLocalScores(variables, m)
    if (constrained) constrainLocal(variables, localScores, LabelDomain)
    ChainCliqueValues(localScores, markovScores.map(_.asInstanceOf[DenseTensor2]))
  }

  def infer(variables: Seq[DiscreteVar], model: Model): BPSummary = {
    val scores = getScores(variables.toSeq, model, constrained = true)
    val res = ChainHelper.viterbiFast(scores)
    val result = ViterbiResults(res.mapScore, res.mapValues, res.scores.localValues)
    val theVariables = variables
    new BPSummary(BPMaxProductRing) {
      _logZ = result.mapScore
      override def logZ: Double = result.mapScore
      override def setToMaximize(implicit d: DiffList): Unit = {
        val labels = theVariables.map(x => {
          x.asInstanceOf[MutableDiscreteVar]
        }).toSeq
        result.setToMax(labels)
      }
    }
  }
}
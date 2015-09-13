package edu.umass.cs.iesl.bibie.model

import cc.factorie._
import cc.factorie.app.nlp.Token
import cc.factorie.la.DenseTensor2
import cc.factorie.model.{DotTemplateWithStatistics2, TemplateModel}
import cc.factorie.variable.{BinaryFeatureVectorVariable, CategoricalVectorDomain}

object CitationFeaturesDomain extends CategoricalVectorDomain[String]

class CitationFeatures(val token: Token) extends BinaryFeatureVectorVariable[String] {
  def domain = CitationFeaturesDomain
  override def skipNonCategories = true
}

class CitationCRFModel extends TemplateModel with Parameters {
  // Factor between label and observed token
  val localTemplate = new DotTemplateWithStatistics2[CitationLabel, CitationFeatures] {
    factorName = "observation"
    val weights = Weights(new DenseTensor2(CitationLabelDomain.size, CitationFeaturesDomain.dimensionSize))
    def unroll1(label: CitationLabel) = Factor(label, label.token.attr[CitationFeatures])
    def unroll2(tf: CitationFeatures) = Factor(tf.token.attr[CitationLabel], tf)
  }
  // Transition factors between two successive labels
  val transitionTemplate = new DotTemplateWithStatistics2[CitationLabel, CitationLabel] {
    factorName = "markov"
    val weights = Weights(new DenseTensor2(CitationLabelDomain.size, CitationLabelDomain.size))
    def unroll1(label: CitationLabel) = if (label.token.sentenceHasPrev) Factor(label.token.sentencePrev.attr[CitationLabel], label) else Nil
    def unroll2(label: CitationLabel) = if (label.token.sentenceHasNext) Factor(label, label.token.sentenceNext.attr[CitationLabel]) else Nil
  }
  this += localTemplate
  this += transitionTemplate
  def sparsity = parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat / parameters.tensors.sumInts(_.length)
}

package edu.umass.cs.iesl.bibie.model

import cc.factorie._
import cc.factorie.app.nlp.{Document, Token, Sentence}
import cc.factorie.la.DenseTensor2
import cc.factorie.model.{DotTemplateWithStatistics2, TemplateModel}

import edu.umass.cs.iesl.bibie.load.PreFeatures
import edu.umass.cs.iesl.bibie.evaluate.SegmentationEvaluation
import edu.umass.cs.iesl.bibie.util.{DefaultLexicons, Lexicons}
import edu.umass.cs.iesl.bibie.segment.OverSegmenter

class CitationCRFModel(lexicons: Lexicons) extends TemplateModel with Parameters {

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

  /* infrastructure for features */
  val featureBuilder = new Features(lexicons)

  def computeDocumentFeatures(doc: Document, training: Boolean): Unit = {
    if (training) {
      val sentenceIter = doc.sentences.toIterator
      while (sentenceIter.hasNext) {
        val sentence = sentenceIter.next()
        if (sentence.size > 0) {
          computeTokenFeatures(sentence)
        }
      }
    } else {
      doc.sentences.filter(_.size > 0).par.foreach { sentence =>
        computeTokenFeatures(sentence)
      }
    }
    featureBuilder.initSentenceFeatures(doc)
  }

  def computeTokenFeatures(sentence: Sentence): Unit = {
    sentence.tokens.foreach { token =>
      token.attr += new CitationFeatures(token)
      featureBuilder.wordToFeatures(token)
    }
  }

  def computeDocumentFeaturesGrobid(doc: Document, training: Boolean): Unit = {
    if (training) {
      val sentenceIter = doc.sentences.toIterator
      while (sentenceIter.hasNext) {
        val sentence = sentenceIter.next()
        if (sentence.size > 0) {
          computeTokenFeaturesGrobid(sentence)
        }
      }
    } else {
      doc.sentences.filter(_.size > 0).par.foreach { sentence =>
        computeTokenFeaturesGrobid(sentence)
      }
    }
  }

  def computeTokenFeaturesGrobid(sentence: Sentence): Unit = {
    sentence.tokens.foreach { token =>
      val features = new CitationFeatures(token)
      if (token.attr[PreFeatures] != null) {
        features ++= token.attr[PreFeatures].features
      }
      token.attr += features
    }
  }

  def computeDocumentFeaturesBoth(doc: Document, training: Boolean): Unit = {
    if (training) {
      val sentenceIter = doc.sentences.toIterator
      while (sentenceIter.hasNext) {
        val sentence = sentenceIter.next()
        if (sentence.size > 0) {
          computeTokenFeaturesBoth(sentence)
        }
      }
    } else {
      doc.sentences.filter(_.size > 0).par.foreach { sentence =>
        computeTokenFeaturesBoth(sentence)
      }
    }
    featureBuilder.initSentenceFeatures(doc)
  }

  def computeTokenFeaturesBoth(sentence: Sentence): Unit = {
    sentence.tokens.foreach { token =>
      token.attr += new CitationFeatures(token)
      featureBuilder.wordToFeatures(token)
      token.attr[CitationFeatures] ++= token.attr[PreFeatures].features
    }
  }

  /* infrastructure for evaluation */
  val evaluator = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)
  def evaluate(docs: Seq[Document], extra: String = ""): Double = {
    evaluator.printEvaluation(docs, extra = extra)
    evaluator.segmentationEvaluation(docs, extra = extra)
  }


}

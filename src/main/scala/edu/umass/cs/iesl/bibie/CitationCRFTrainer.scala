package edu.umass.cs.iesl.bibie

import java.io._
import java.net.URL

import cc.factorie._
import cc.factorie.app.nlp.{TokenSpan, Document, Token}
import cc.factorie.app.strings._
import cc.factorie.la.{DenseTensor2, Tensor2}
import cc.factorie.model.DotTemplateWithStatistics2
import cc.factorie.optimize.{L2Regularization, LBFGS, LikelihoodExample, ThreadLocalBatchTrainer}
import cc.factorie.util.{BinarySerializer}
import cc.factorie.variable._

object LabelDomain extends CategoricalDomain[String]

abstract class CLabel(labelname: String) extends LabeledCategoricalVariable(labelname)

class CitationLabel(labelname: String, val token: Token) extends CLabel(labelname) {
  def domain = LabelDomain
  def hasNext = token.hasNext && token.next != null
  def hasPrev = token.hasPrev && token.prev != null
  def next = token.next
  def prev = token.prev
}

object CitationFeaturesDomain extends CategoricalVectorDomain[String]

class CitationFeatures(val token: Token) extends BinaryFeatureVectorVariable[String] {
  def domain = CitationFeaturesDomain
  override def skipNonCategories = true
}

object SpanLabelDomain extends CategoricalDomain[String]

class SpanCitationLabel(val span: CitationSpan, initialValue: String) extends CLabel(initialValue) {
  def domain = SpanLabelDomain
}

class SegmentSpan(doc: Document, start: Int, length: Int, var latch: Boolean = false)(implicit d: DiffList) extends TokenSpan(doc.asSection, start, length) {
  var latchLabel = ""
  var cantBegin = false
  var cantEnd = false
  val restrict = new collection.mutable.HashMap[String, Boolean]()
}

class CitationSpan(doc: Document, labelString: String, start: Int, length: Int) extends TokenSpan(doc.asSection, start, length) {
  val label = new SpanCitationLabel(this, labelString)
  override def toString() = "CitationSpan(" + length + "," + label.categoryValue + ":" + this.string + ")"
}

class CitationCRFModel extends TemplateModel with Parameters {
  // Factor between label and observed token
  val localTemplate = new DotTemplateWithStatistics2[CitationLabel, CitationFeatures] {
    factorName = "observation"
    val weights = Weights(new DenseTensor2(LabelDomain.size, CitationFeaturesDomain.dimensionSize))
    def unroll1(label: CitationLabel) = Factor(label, label.token.attr[CitationFeatures])
    def unroll2(tf: CitationFeatures) = Factor(tf.token.attr[CitationLabel], tf)
  }
  // Transition factors between two successive labels
  val transitionTemplate = new DotTemplateWithStatistics2[CitationLabel, CitationLabel] {
    factorName = "markov"
    val weights = Weights(new DenseTensor2(LabelDomain.size, LabelDomain.size))
    def unroll1(label: CitationLabel) = if (label.token.sentenceHasPrev) Factor(label.token.sentencePrev.attr[CitationLabel], label) else Nil
    def unroll2(label: CitationLabel) = if (label.token.sentenceHasNext) Factor(label, label.token.sentenceNext.attr[CitationLabel]) else Nil
  }
  this += localTemplate
  this += transitionTemplate
}

class CitationCRFTrainer {
  val evaluator = new SegmentationEvaluation[CitationLabel](LabelDomain)

  // was this an untrained model being used? we should remove -luke
  var model = new CitationCRFModel
  var lexicons: Lexicons = null
  var loc = 0

  // TODO: add some global features to baseline CRF - like "containsthesis"

  def wordToFeatures(token: Token) {

    val docSpans = token.document.attr[CitationSpanList].spans
    val features = token.attr += new CitationFeatures(token)
    val word = token.string
    val email = """([\w\d\-\_]+)(\+\d+)?@([\w\d\-\.]+)"""
    val url = """((http|ftp|https):\/\/)?[\w\-_]+(\.[\w\-_]+)+([\w\-\.,@?^=%&amp;:/~\+#]*[\w\-\@?^=%&amp;/~\+#])?"""
    val lower = word.toLowerCase
    val replace = lower.replaceAll("\\.|,|\\)", "")
    token.attr[CitationFeatures] += "W=" + word
    token.attr[CitationFeatures] += "SIMPLE=" + simplifyDigits(word)
    token.attr[CitationFeatures] += "SIMPLE_LOWER=" + simplifyDigits(word).toLowerCase
    if (word.length > 3) token.attr[CitationFeatures] += "PRE=" + word.substring(0, 3)
    if (word.matches(Capitalized)) token.attr[CitationFeatures] += "CAPITALIZED"
    if (word.matches(AllCaps)) token.attr[CitationFeatures] += "ALLCAPS"
    if (word.matches(Numeric)) token.attr[CitationFeatures] += "NUMERIC"
    if (word.matches(ParenNumeric)) token.attr[CitationFeatures] += "PARENNUMERIC"
    if (word.matches(Punctuation)) token.attr[CitationFeatures] += "PUNCTUATION"
    if (ContainsDigit.findFirstMatchIn(word) != None) token.attr[CitationFeatures] += "CONTAINSDIGIT"
    if (word.contains(".")) token.attr[CitationFeatures] += "CONTAINSDOTS"
    if (word.contains("-")) token.attr[CitationFeatures] += "CONTAINSDASH"
    if (word.matches("[0-9]+\\-[0-9]+")) token.attr[CitationFeatures] += "POSSIBLEPAGES"
    if (word.matches("[A-Z]")) token.attr[CitationFeatures] += "CAPLETTER"
    if (word.matches("[a-zA-Z]")) token.attr[CitationFeatures] += "SINGLECHAR"
    if (word.matches("[A-Z]\\.")) token.attr[CitationFeatures] += "LONLEYINITIAL"
    if (word.matches(email)) token.attr[CitationFeatures] += "EMAIL"
    if (word.matches(url)) token.attr[CitationFeatures] += "URL"
    if (word.matches(EndComma)) token.attr[CitationFeatures] += "ENDCOMMA"
    if (word.matches(EndPeriod)) token.attr[CitationFeatures] += "ENDPERIOD"
    if (word.matches(EndFullColon)) token.attr[CitationFeatures] += "ENDFULLCOLON"
    if (word.matches(HasOpenParen)) token.attr[CitationFeatures] += "OPENPAREN"
    if (word.matches(HasClosedParen)) token.attr[CitationFeatures] += "CLOSEDPAREN"
    if (word.matches(HasOpenParen) && word.matches(HasClosedParen)) token.attr[CitationFeatures] += "OPENANDSHUT"
    if (word.matches(HasOpenSquare)) token.attr[CitationFeatures] += "OPENSQUARE"
    if (word.matches(HasClosedSquare)) token.attr[CitationFeatures] += "CLOSEDSQUARE"
    if (word.matches(".*[0-9]$")) token.attr[CitationFeatures] += "LASTNUM"
    if (word.matches(".*[A-Z]$")) token.attr[CitationFeatures] += "LASTUPPER"
    if (word.matches(".*[a-z]$")) token.attr[CitationFeatures] += "LASTLOWER"
    if (word.matches(".*\"$")) token.attr[CitationFeatures] += "ENDQUOTE"
    if (word.matches(".*\".$")) token.attr[CitationFeatures] += "QUOTEATEND"
    if (word.matches(".*;$")) token.attr[CitationFeatures] += "ENDSEMI"
    if (word.matches("^\".*")) token.attr[CitationFeatures] += "BEGINQUOTE"
    if (word.matches(".*\\\\")) token.attr[CitationFeatures] += "ENDFORWARD"
    if (word.matches("^\\\\.*")) token.attr[CitationFeatures] += "BEGINFORWARD"
    if (word.matches(".*,\"$")) token.attr[CitationFeatures] += "ENDCOMMAQUOTE"
    if (word.matches("^[\\'`].*")) token.attr[CitationFeatures] += "STARTSINGLEQUOTE"
    if (word.matches(".*'.?$")) token.attr[CitationFeatures] += "ENDSINGLEQUOTE"
    if (word.trim.toLowerCase == "and" || word.trim.toLowerCase == "&") token.attr[CitationFeatures] += "ISAND"
    if (word.matches(".*[1-9](th|nd|st).*")) token.attr[CitationFeatures] += "EVENTITERATION"
    if (word.trim.toLowerCase == "thesis" || word.trim.toLowerCase == "dissertation") token.attr[CitationFeatures] += "ISTHESIS"
    val counts = count(word)
    token.attr[CitationFeatures] += "NUMDIGITS=" + counts._1
    token.attr[CitationFeatures] += "NUMALPHA=" + counts._2
    token.attr[CitationFeatures] += "NUMDIGITS=" + counts._1 + "ALPHS=" + counts._2
    if (replace.matches(Numeric) && counts._1 == 4 && replace.toInt < 1900) token.attr[CitationFeatures] += "BEFORE1900"
    if (replace.matches(Numeric) && counts._1 == 4 && replace.toInt >= 1900) token.attr[CitationFeatures] += "AFTER1900"
    //if (word.matches(ParenNumeric) && counts._1 == 4 && word.replaceFirst("\\).?$",")").replaceAll("[\\)\\(]","").toInt < 1900) token.attr[CitationFeatures] += "BEFORE1900"
    //if (word.matches(ParenNumeric) && counts._1 == 4 && word.replaceFirst("\\).?$",")").replaceAll("[\\)\\(]","").toInt >= 1900) token.attr[CitationFeatures] += "AFTER1900"
    if (lower.startsWith("appeared") || lower.startsWith("submitted") || lower.startsWith("appear")) token.attr[CitationFeatures] += "STATUS"
    //if(token.startsSpansOfClass[SegmentSpan].nonEmpty) token.attr[CitationFeatures] += "PROBABLESEGMENT"
    if (docSpans.exists(span => span.tokens.head == token)) token.attr[CitationFeatures] += "PROBABLESEGMENT"
    if (lower.matches("(ed\\.|eds\\.|editor|editors).*")) token.attr[CitationFeatures] += "EDITOR"
    if (lower.matches("(proc\\.?|proceedings|trans\\.?|conf\\.?|symp\\.?|conference|symposium|workshop).*")) token.attr[CitationFeatures] += "BOOKTITLE"
    if (lower.matches("(university|dept\\.|department).*")) token.attr[CitationFeatures] += "INST"
    if (lower.matches("^p(p|ages|pps|gs)?\\.?")) token.attr[CitationFeatures] += "ISPAGES"
    if (lower.matches("(v\\.?|volume|vol\\.?).*")) token.attr[CitationFeatures] += "VOLUME"
    loc += 1
  }


  def count(string: String): (Int, Int) = {
    var digits = 0
    var alpha = 0
    for (char <- string) {
      if (char.toString.matches("[0-9]")) digits += 1
      else if (char.toString.matches("[a-zA-Z]")) alpha += 1
    }
    (digits, alpha)
  }

  def initSentenceFeatures(data: Seq[Document]) {
    for (d <- data) {
      val docLength = d.tokens.size
      for (token <- d.tokens) {
        val per = token.position.toDouble / docLength.toDouble
        val binned = ((per * 60.0) / 5.0).floor
        token.attr[CitationFeatures] += "BIN=" + binned
      }
    }

    for (d <- data) {
      for (t <- d.tokens) {
        if (t.nextWindow(10).count(_.string.toLowerCase.matches(".*(ed\\.|editor|eds|editors).*")) > 0) t.attr[CitationFeatures] += "POSSIBLEEDITOR"
      }
    }

    for (d <- data) {
      for (s <- d.sentences) {
        for (token <- s.tokens) {
          for (lexicon <- lexicons(token)) {
            token.attr[CitationFeatures] += "LEX=" + lexicon
          }
        }
        cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(s.tokens, (t: Token) => t.attr[CitationFeatures], "^[^@]*$", List(0, 0), List(1), List(2), List(-1), List(-2))
      }
    }
  }

  val Capitalized = "^[A-Z].*"
  val AllCaps = "^[A-Z]*"
  val Numeric = "^[0-9]+$"
  val ParenNumeric = "^\\([0-9]+\\).?$"
  val Punctuation = "[-,\\.;:?!()]+"
  val EndPeriod = ".*\\.$"
  val EndFullColon = ".*\\:$"
  val EndComma = ".*\\,$"
  val HasOpenParen = ".*\\(.*"
  val HasClosedParen = ".*\\).*"
  val HasOpenSquare = ".*\\[.*"
  val HasClosedSquare = ".*\\].*"
  val ContainsDigit = ".*[0-9].*".r

  def tag(token: Token): String = {
    token.attr[CitationLabel].categoryValue.substring(2)
  }

  def test(testDocuments: Seq[Document]) {
    for (d <- testDocuments) {
      d.tokens.foreach(t => wordToFeatures(t))
    }
    initSentenceFeatures(testDocuments)

    val testLabels: Seq[CitationLabel] = testDocuments.flatMap(_.tokens).map(_.attr[CitationLabel]).toSeq

    testLabels.foreach(_.setRandomly(random))

    testDocuments.foreach(process)

    evaluator.printEvaluation(testDocuments, testDocuments, "FINAL")
    evaluator.segmentationEvaluation(testDocuments, testDocuments, "FINAL")

    println("Test Documents")
    testDocuments.foreach(CitationCRFTrainer.printDocument)
  }

  def train(trainDocuments: Seq[Document], testDocuments: Seq[Document]): Unit = {
    // Read in the data
    // Add features for NER
    implicit val random = new scala.util.Random
    println("Num TokenFeatures = " + CitationFeaturesDomain.dimensionDomain.size)

//    trainDocuments.take(1).foreach(d => d.tokens.foreach(t => println(t.attr[CitationFeatures])))
//    testDocuments.take(1).foreach(d => d.tokens.foreach(t => println(t.attr[CitationFeatures])))

    trainDocuments.take(5).foreach {d =>
      CitationCRFTrainer.printDocument(d)
    }

    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels: Seq[CitationLabel] = trainDocuments.flatMap(_.tokens).map(_.attr[CitationLabel]).toSeq
    val testLabels: Seq[CitationLabel] = testDocuments.flatMap(_.tokens).map(_.attr[CitationLabel]).toSeq
    (trainLabels ++ testLabels).filter(_ != null).foreach(_.setRandomly(random))

    val vars = for (td <- trainDocuments; sentence <- td.sentences if sentence.length > 1) yield sentence.tokens.map(_.attr[CitationLabel])

    val examples = vars.map(v => new LikelihoodExample(v.toSeq, model, cc.factorie.infer.InferByBPChain))
    val trainer = new ThreadLocalBatchTrainer(model.parameters, new LBFGS with L2Regularization)
    trainer.trainFromExamples(examples)

    (trainLabels ++ testLabels).foreach(_.setRandomly(random))

    trainDocuments.foreach(process)
    testDocuments.foreach(process)

    trainDocuments.take(5).foreach {d =>
      CitationCRFTrainer.printDocument(d)
    }

    testDocuments.take(5).foreach {d =>
      CitationCRFTrainer.printDocument(d)
    }

    evaluator.printEvaluation(testDocuments, testDocuments, "FINAL")
  }

  def process(document: Document): Seq[Seq[String]] = {
    if (document.tokens.size == 0) return null.asInstanceOf[Seq[Seq[String]]]
    for (sentence <- document.sentences if sentence.tokens.size > 0) {
      val vars = sentence.tokens.map(_.attr[CitationLabel]).toSeq
      val sum = CitationBIOHelper.infer(vars, model)
      sum.setToMaximize(null)
    }
    // TODO why is this here?? -luke
    Seq[Seq[String]]()
  }


  //  def table(documents: Iterable[Document]) {
  //    val table = new scala.collection.mutable.HashMap[(String, String), Int]()
  //    for (d <- documents; t <- d.tokens; if t.attr[CitationLabel].target.categoryValue.startsWith("B-")) {
  //      val b = t.attr[CitationLabel].categoryValue.replaceAll("(I-|B-)", "")
  //      val ta = t.attr[CitationLabel].target.categoryValue.replaceAll("(I-|B-)", "")
  //      if (table.contains((b, ta)))
  //        table((b, ta)) += 1
  //      else
  //        table((b, ta)) = 1
  //    }
  //    val sorted = table.toSeq.filter(a => a._1._1 != a._1._2).sortBy(-_._2).take(30)
  //    for (s <- sorted) {
  //      println(s._1._1 + " -> " + s._1._2 + " : " + s._2)
  //    }
  //
  //  }

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataOutputStream(stream)
    BinarySerializer.serialize(LabelDomain, is)
    BinarySerializer.serialize(CitationFeaturesDomain.dimensionDomain, is)
    BinarySerializer.serialize(model, is)
    is.close()
  }
  def deserialize(stream: InputStream): Unit = deserialize(model, stream)

  def deserialize(theModel: CitationCRFModel, stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(stream)
    BinarySerializer.deserialize(LabelDomain, is)
    LabelDomain.freeze()
    println("deserialized labeldomain")
    BinarySerializer.deserialize(CitationFeaturesDomain.dimensionDomain, is)
    CitationFeaturesDomain.freeze()
    println("deserialized featuresdomain")

    BinarySerializer.deserialize(theModel, is)
    println("deserialized model")
    is.close()
  }

}


object CitationCRFTrainer extends CitationCRFTrainer {
  var verbose = false

  def tag(label: String): String = {
    if (label == "O") "O"
    else label //.substring(2)
  }

  def printDocument(document: Document): Unit = {
    var current = ""
    for (token <- document.tokens; if token.attr.contains[CitationLabel]) {
      if (tag(token.attr[CitationLabel].categoryValue) != current) {
        var label = tag(token.attr[CitationLabel].categoryValue)
        if (current != "") {
          print("</" + current + ">")
        }
        print("<" + label + ">")
        current = label
      }
      print(token.string)
      print(" ")
    }
    println
    current = ""
    var error = false
    var span = ""
    for (token <- document.tokens; if token.attr.contains[CitationLabel]) {
      if (tag(token.attr[CitationLabel].target.categoryValue) != current) {
        var label = tag(token.attr[CitationLabel].target.categoryValue)
        if (current != "") {
          if (error) print("*") else print(" ")
          println(span)
        }
        error = false
        if (label != tag(token.attr[CitationLabel].target.categoryValue)) error = true
        current = label
        span = "\t" + current + "\t"
      }
      if (tag(token.attr[CitationLabel].categoryValue) != tag(token.attr[CitationLabel].target.categoryValue)) error = true
      span += " " + token.string
    }
    if (error) print("*") else print(" ")
    println(span)
    println("TrueValue")
    current = ""
    for (token <- document.tokens; if token.attr.contains[CitationLabel]) {
      if (tag(token.attr[CitationLabel].target.categoryValue) != current) {
        var label = tag(token.attr[CitationLabel].target.categoryValue)
        if (current != "") {
          print("</" + current + ">")
        }
        print("<" + label + ">")
        current = label
      }
      print(token.string)
      print(" ")
    }
    println("</" + current + ">")
    printDocument2(document)
  }

  def printDocument2(document: Document) {
    var current = ""
    println("predicted: ")
    current = ""
    var error = false
    var span = ""
    for (token <- document.tokens; if token.attr.contains[CitationLabel]) {
      if (tag(token.attr[CitationLabel].categoryValue) != current) {
        val label = tag(token.attr[CitationLabel].categoryValue)
        if (current != "") {
          if (error) print("*") else print(" ")
          println(span)
        }
        error = false
        if (label != tag(token.attr[CitationLabel].categoryValue)) error = true
        current = label
        span = "\t" + current + "\t"
      }
      if (tag(token.attr[CitationLabel].categoryValue) != tag(token.attr[CitationLabel].target.categoryValue)) error = true
      span += " " + token.string
    }
    if (error) print("*") else print(" ")
    println(span)
  }

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
}
//
//
//object CitationCRFTester extends App {
//  val crf = new CitationCRFTrainer
//  crf.deSerialize(new FileInputStream("LinearChainModel"))
//  LabelDomain.freeze()
//  CitationFeaturesDomain.freeze()
//  val testingData = LoadHier.fromFile(args(0))
//  OverSegmenter.overSegment(testingData)
//  println(" Testing: " + testingData.size)
//  val lexes = List("institution.lst", "tech.lst", "note.lst", "WikiLocations.lst", "WikiLocationsRedirects.lst", "WikiOrganizations.lst", "WikiOrganizationsRedirects.lst", "cardinalNumber.txt", "known_corporations.lst", "known_country.lst", "known_name.lst", "known_names.big.lst",  "known_state.lst",  "temporal_words.txt", "authors.lst",  "journal.lst", "names.lst", "publishers.lst")
//  crf.lexicons = new prlearn.Lexicons("src/main/resources/lexicons", lexes)
//  crf.errors(testingData,true)
//}

//object CitationTrainer {
//  def main(args: Array[String]) {
//    val trainer = new CitationCRFTrainer
//    val trainingData = LoadHier.fromFile(args(0))
//    val testingData = LoadHier.fromFile(args(1))
//    OverSegmenter.overSegment(trainingData ++ testingData)
//
//    println("Training: " + trainingData.size + " Testing: " + testingData.size)
//    val lexes = List("institution.lst", "tech.lst", "note.lst", "WikiLocations.lst", "WikiLocationsRedirects.lst", "WikiOrganizations.lst", "WikiOrganizationsRedirects.lst", "cardinalNumber.txt", "known_corporations.lst", "known_country.lst", "known_name.lst", "known_names.big.lst", "known_state.lst", "temporal_words.txt", "authors.lst", "journal.lst", "names.lst", "publishers.lst")
//    trainer.lexicons = new Lexicons("src/main/resources/lexicons", lexes)
//    for (d <- trainingData) {
//      d.tokens.foreach(t => trainer.wordToFeatures(t))
//    }
//    trainer.initSentenceFeatures(trainingData)
//    CitationFeaturesDomain.freeze()
//    for (d <- testingData) {
//      d.tokens.foreach(t => trainer.wordToFeatures(t))
//    }
//    trainer.initSentenceFeatures(testingData)
//
//    trainer.train(trainingData, testingData)
//    trainer.serialize(new FileOutputStream("LinearChainModel"))
//  }
//}


// TODO: add DocumentAnnotator to add citations

object TrainCitationModelGrobid {
  URLHandlerSetup.poke()
  def main(args: Array[String]): Unit = {
    println("TrainCitationModelGrobid")

    val opts = new TrainCitationModelOptions
    opts.parse(args)
    val trainer = new CitationCRFTrainer
    val trainingData = LoadGrobid.fromDir(opts.trainDir.value)

    val exDoc = trainingData.head
    exDoc.tokens.foreach { t => println(s"${t.string}\t${t.attr[CitationLabel].categoryValue}") }

    val testingData = LoadGrobid.fromDir(opts.testDir.value)
    val lexiconDir = opts.lexiconUrl.value
    OverSegmenter.overSegment(trainingData ++ testingData, lexiconDir)

    println("Training: " + trainingData.size + " Testing: " + testingData.size)
    val lexes = List("institution.lst", "tech.lst", "note.lst", "WikiLocations.lst", "WikiLocationsRedirects.lst", "WikiOrganizations.lst", "WikiOrganizationsRedirects.lst",
      "cardinalNumber.txt", "known_corporations.lst", "known_country.lst", "known_name.lst", "known_names.big.lst", "known_state.lst", "temporal_words.txt", "authors.lst",
      "journal.lst", "names.lst", "publishers.lst")
    trainer.lexicons = new Lexicons(lexiconDir, lexes)
    for (d <- trainingData)
      d.tokens.foreach(trainer.wordToFeatures)
    trainer.initSentenceFeatures(trainingData)
    CitationFeaturesDomain.freeze()
    for (d <- testingData)
      d.tokens.foreach(trainer.wordToFeatures)
    trainer.initSentenceFeatures(testingData)

    trainer.train(trainingData, testingData)

    if (opts.saveModel.value) trainer.serialize(new FileOutputStream(opts.modelFile.value))
  }
}


object TrainCitationModel {
  URLHandlerSetup.poke()
  def main(args: Array[String]): Unit = {
    val opts = new TrainCitationModelOptions
    opts.parse(args)
    val trainer = new CitationCRFTrainer
    val trainingData = LoadHier.fromFile(opts.trainFile.value).take(5)
    val testingData = if (opts.testFile.value.isEmpty) Seq() else LoadHier.fromFile(opts.testFile.value).take(2)

    val lexiconDir = opts.lexiconUrl.value
    OverSegmenter.overSegment(trainingData ++ testingData, lexiconDir)

    println("Training: " + trainingData.size + " Testing: " + testingData.size)
    val lexes = List("institution.lst", "tech.lst", "note.lst", "WikiLocations.lst", "WikiLocationsRedirects.lst", "WikiOrganizations.lst", "WikiOrganizationsRedirects.lst",
      "cardinalNumber.txt", "known_corporations.lst", "known_country.lst", "known_name.lst", "known_names.big.lst", "known_state.lst", "temporal_words.txt", "authors.lst",
      "journal.lst", "names.lst", "publishers.lst")
    trainer.lexicons = new Lexicons(lexiconDir, lexes)
    for (d <- trainingData)
      d.tokens.foreach(trainer.wordToFeatures)
    trainer.initSentenceFeatures(trainingData)
    CitationFeaturesDomain.freeze()
    for (d <- testingData)
      d.tokens.foreach(trainer.wordToFeatures)
    trainer.initSentenceFeatures(testingData)

    trainer.train(trainingData, testingData)

    if (opts.saveModel.value)
      trainer.serialize(new FileOutputStream(opts.modelFile.value))
  }
}

object URLHandlerSetup {
  System.setProperty("java.protocol.handler.pkgs", "bibie.protocols")
  // make sure this gets instantiated
  def poke(): Unit = {
    println("attaching resource url handler.")
  }
}

object TestCitationModel {
  URLHandlerSetup.poke()
  def loadModel(modelUrl: String, lexiconUrlPrefix: String = "classpath:lexicons"): CitationCRFTrainer = {
    val trainer = new CitationCRFTrainer
    trainer.deserialize(new URL(modelUrl).openStream())
    val lexes = List("institution.lst", "tech.lst", "note.lst", "WikiLocations.lst", "WikiLocationsRedirects.lst", "WikiOrganizations.lst", "WikiOrganizationsRedirects.lst",
      "cardinalNumber.txt", "known_corporations.lst", "known_country.lst", "known_name.lst", "known_names.big.lst", "known_state.lst", "temporal_words.txt", "authors.lst",
      "journal.lst", "names.lst", "publishers.lst")
    trainer.lexicons = new Lexicons(lexiconUrlPrefix, lexes)
    trainer
  }
  def process(docs: Seq[Document], model: CitationCRFTrainer, print: Boolean = true): Unit = {
    OverSegmenter.overSegment(docs, model.lexicons.urlPrefix)
    for (d <- docs)
      d.tokens.foreach(model.wordToFeatures)
    model.initSentenceFeatures(docs)
    docs.foreach(model.process)
    if (print)
      model.evaluator.printEvaluation(docs, docs, "FINAL")
  }
  def main(args: Array[String]): Unit = {
    val opts = new TestCitationModelOptions
    opts.parse(args)

    val model = loadModel(opts.modelUrl.value, opts.lexiconUrl.value)
    val testingData = LoadHier.fromFile(opts.testFile.value)

    process(testingData, model)
  }
}

class TrainCitationModelOptions extends cc.factorie.util.DefaultCmdOptions with cc.factorie.app.nlp.SharedNLPCmdOptions {
  val trainFile = new CmdOption("train-file", "", "STRING", "UMass formatted training file.")
  val trainDir = new CmdOption("train-dir", "", "STRING", "path to directory of training files")
  val testFile = new CmdOption("test-file", "", "STRING", "UMass formatted testing file (or blank).")
  val testDir = new CmdOption("test-dir", "", "STRING", "path to directory of testing files")
  val modelFile = new CmdOption("model-file", "citationCRF.factorie", "STRING", "File for saving model.")
  val lexiconUrl = new CmdOption("lexicons", "classpath:lexicons", "STRING", "URL prefix for lexicon files/resources named cities, companies, companysuffix, countries, days, firstname.high, ...")
  val saveModel = new CmdOption("save-model", true, "BOOLEAN", "Whether to save the model or just train/test.")
  val rate = new CmdOption("adagrad-rate", 1.0, "FLOAT", "Adagrad learning rate.")
  val delta = new CmdOption("adagrad-delta", 0.1, "FLOAT", "Adagrad delta (ridge).")
}

class TestCitationModelOptions extends cc.factorie.util.DefaultCmdOptions with cc.factorie.app.nlp.SharedNLPCmdOptions {
  val testFile = new CmdOption("test-file", "", "STRING", "UMass formatted testing file.")
  val modelUrl = new CmdOption("model-url", "file://citationCRF.factorie", "STRING", "URL for loading model.")
  val lexiconUrl = new CmdOption("lexicons", "classpath:lexicons", "STRING", "URL prefix for lexicon files/prefixes named cities, companies, companysuffix, countries, days, firstname.high, ...")
}

//class CitationOpts extends DefaultCmdOptions {
//  val train = new CmdOption("train", "", "FILE", "CoNLL formatted training file.")
//  val test = new CmdOption("test", "", "FILE", "CoNLL formatted test file.")
//  val modelDir = new CmdOption("model", "citationCRF.factorie", "DIR", "Directory for saving or loading model.")
//  val lexiconDir = new CmdOption("lexicons", "", "DIR", "Directory containing lexicon files named cities, companies, companysuffix, countries, days, firstname.high,...")
//  val saveModel = new CmdOption("save-model", false, "BOOLEAN", "Whether to save the model")
//  val runOnlyHere = new CmdOption("runOnlyHere", false, "BOOLEAN", "Whether just optimize")
//  val rate = new CmdOption("rate", 1.0, "Double", "Rate for AdaGrad.")
//  val delta = new CmdOption("delta", 1.0, "Double", "Delta for AdaGrad.")
//  val baseLrate = new CmdOption("baseLrate", 1.0, "Double", "Base lambda value for DD.")
//  val lRateExp = new CmdOption("lRateExp", 1.0, "Double", "Rate for sub for DD.")
//}

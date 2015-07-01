package edu.umass.cs.iesl.bibie

import java.io._
import java.net.URL

import cc.factorie._
import cc.factorie.app.chain.SegmentEvaluation
import cc.factorie.app.nlp.{TokenSpan, Document, Token}
import cc.factorie.app.strings._
import cc.factorie.la.DenseTensor2
import cc.factorie.model.DotTemplateWithStatistics2
import cc.factorie.optimize.{L2Regularization, LBFGS, LikelihoodExample, ThreadLocalBatchTrainer, AdaGradRDA, Trainer}
import cc.factorie.util._
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
  val model = new CitationCRFModel
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

  def tag(token: Token): String = token.attr[CitationLabel].categoryValue.substring(2)
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

  def train(trainDocuments: Seq[Document], testDocuments: Seq[Document], params: Hyperparams): Unit = {
    implicit val random = new scala.util.Random
    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels: Seq[CitationLabel] = trainDocuments.flatMap(_.tokens).map(_.attr[CitationLabel]).toSeq
    val testLabels: Seq[CitationLabel] = testDocuments.flatMap(_.tokens).map(_.attr[CitationLabel]).toSeq
    (trainLabels ++ testLabels).filter(_ != null).foreach(_.setRandomly(random))
    val vars = for (td <- trainDocuments; sentence <- td.sentences if sentence.length > 1) yield sentence.tokens.map(_.attr[CitationLabel])
    val examples = vars.map(v => new LikelihoodExample(v.toSeq, model, cc.factorie.infer.InferByBPChain))
    def evaluate(): Unit = {
      trainDocuments.foreach(process)
      testDocuments.foreach(process)
      evaluator.printEvaluation(trainDocuments, testDocuments, "TRAINING")
    }
    params.optimizer match {
      case "lbfgs" =>
        val optimizer = new LBFGS with L2Regularization
        val trainer = new ThreadLocalBatchTrainer(model.parameters, optimizer)
        println("training with LBFGS ...")
        trainer.trainFromExamples(examples)
      case "adagrad" =>
        val optimizer = new AdaGradRDA(delta=params.delta, rate=params.rate, l1=params.l1, l2=params.l2, numExamples=examples.length)
        println("training with AdaGradRDA ...")
        Trainer.onlineTrain(model.parameters, examples, evaluate=evaluate, useParallelTrainer=false, maxIterations=1, optimizer=optimizer)
      case _ => throw new Exception(s"invalid optimizer: ${params.optimizer}")
    }
    (trainLabels ++ testLabels).foreach(_.setRandomly(random))
    trainDocuments.foreach(process)
    testDocuments.foreach(process)
    val tot = model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat
    val len = model.parameters.tensors.sumInts(_.length)
    val sparsity = tot / len
    println(s"model sparsity: ${sparsity}")
    evaluator.printEvaluation(trainDocuments, testDocuments, "FINAL")
  }

  def process(document: Document): Document = {
    if (document.tokens.size == 0) return document
    for (sentence <- document.sentences if sentence.tokens.size > 0) {
      val vars = sentence.tokens.map(_.attr[CitationLabel]).toSeq
      val sum = CitationBIOHelper.infer(vars, model)
      sum.setToMaximize(null)
    }
    document
  }

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


// TODO: add DocumentAnnotator to add citations

object TrainCitationModel extends HyperparameterMain {
  URLHandlerSetup.poke()
  def evaluateParameters(args: Array[String]): Double = {
    println(args.mkString(", "))
    val opts = new TrainCitationModelOptions
    opts.parse(args)
    opts.dataSet.value match {
      case "grobid" => trainGrobid(opts)
      case _ => trainDefault(opts)
    }
  }

  def trainDefault(opts: TrainCitationModelOptions): Double = {
    val params = new Hyperparams(opts)
    val trainer = new CitationCRFTrainer
    val trainingData = LoadHier.fromFile(opts.trainFile.value)
    println("before:")
    trainingData.head.tokens.take(10).foreach(t => println(s"${t.string} ${t.attr[CitationLabel].categoryValue}"))
    CitationCRFTrainer.printDocument(trainingData.head)
    val testingData = if (opts.testFile.value.isEmpty) Seq() else LoadHier.fromFile(opts.testFile.value)
    val lexiconDir = opts.lexiconUrl.value
    OverSegmenter.overSegment(trainingData ++ testingData, lexiconDir)
    println("Training: " + trainingData.size + " Testing: " + testingData.size)
    val lexes = List("institution.lst", "tech.lst", "note.lst", "WikiLocations.lst", "WikiLocationsRedirects.lst", "WikiOrganizations.lst", "WikiOrganizationsRedirects.lst",
      "cardinalNumber.txt", "known_corporations.lst", "known_country.lst", "known_name.lst", "known_names.big.lst", "known_state.lst", "temporal_words.txt", "authors.lst",
      "journal.lst", "names.lst", "publishers.lst")
    trainer.lexicons = new Lexicons(lexiconDir, lexes)
    for (d <- trainingData) d.tokens.foreach(trainer.wordToFeatures)
    trainer.initSentenceFeatures(trainingData)
    CitationFeaturesDomain.freeze()
    for (d <- testingData) d.tokens.foreach(trainer.wordToFeatures)
    trainer.initSentenceFeatures(testingData)
    trainer.train(trainingData, testingData, params)
    if (opts.saveModel.value) trainer.serialize(new FileOutputStream(opts.modelFile.value))
    val evaluator = new ExactlyLikeGrobidEvaluator(opts.rootDir.value)
    val (f0, eval) = evaluator.evaluate(testingData, opts.outputFile.value, writeFiles=opts.writeEvals.value, outputDir=opts.outputDir.value)
    println(eval)
    println("\nafter:")
    trainingData.head.tokens.take(10).foreach(t => println(s"${t.string} ${t.attr[CitationLabel].categoryValue}"))
    CitationCRFTrainer.printDocument(trainingData.head)
    f0
  }

  def trainGrobid(opts: TrainCitationModelOptions): Double = {
    def initGrobidFeatures(docs: Seq[Document]): Unit = {
      docs.flatMap(_.tokens).foreach { token =>
        token.attr += new CitationFeatures(token)
        token.attr[CitationFeatures] ++= token.attr[PreFeatures].features
      }
    }
    val params = new Hyperparams(opts)
    val trainer = new CitationCRFTrainer
    val allData = LoadGrobid.fromFilename(opts.trainFile.value, withFeatures=opts.useGrobidFeatures.value)
    val trainPortion = (allData.length.toDouble * opts.trainPortion.value).floor.toInt
    val trainingData = allData.take(trainPortion)
    //TODO print (train, dev) sizes
    //TODO feature domain trimBelowCount?

    val testData = LoadGrobid.fromFilename(opts.testFile.value, withFeatures=opts.useGrobidFeatures.value)

    //    println("before:")
//    trainingData.head.tokens.take(10).foreach(t => println(s"${t.string} ${t.attr[CitationLabel].categoryValue}"))
//    CitationCRFTrainer.printDocument(trainingData.head)
    val devData = allData.drop(trainPortion)
//    val devData = testData


    if (opts.useGrobidFeatures.value) {
      initGrobidFeatures(trainingData)
//      CitationFeaturesDomain.freeze()
    } //else {
      val lexiconDir = opts.lexiconUrl.value
      OverSegmenter.overSegment(trainingData ++ devData ++ testData, lexiconDir) //TODO don't think we need this here? -ks
      println("Training: " + trainingData.size + " Testing: " + devData.size)
      val lexes = List("institution.lst", "tech.lst", "note.lst", "WikiLocations.lst", "WikiLocationsRedirects.lst", "WikiOrganizations.lst", "WikiOrganizationsRedirects.lst",
        "cardinalNumber.txt", "known_corporations.lst", "known_country.lst", "known_name.lst", "known_names.big.lst", "known_state.lst", "temporal_words.txt", "authors.lst",
        "journal.lst", "names.lst", "publishers.lst")
      trainer.lexicons = new Lexicons(lexiconDir, lexes)
      for (d <- trainingData) d.tokens.foreach(trainer.wordToFeatures)
      trainer.initSentenceFeatures(trainingData)
      CitationFeaturesDomain.freeze()
    if (opts.useGrobidFeatures.value) {
      initGrobidFeatures(devData)
    }
      for (d <- devData) d.tokens.foreach(trainer.wordToFeatures)
      trainer.initSentenceFeatures(devData)
//    }
    println(s"feature domain size: ${CitationFeaturesDomain.dimensionDomain.size}")
    trainer.train(trainingData, devData, params)
    if (opts.saveModel.value) trainer.serialize(new FileOutputStream(opts.modelFile.value))
//    val evaluator = new ExactlyLikeGrobidEvaluator(opts.rootDir.value)
//    val (f0, eval) = evaluator.evaluate(devData, opts.outputFile.value, writeFiles=opts.writeEvals.value, outputDir=opts.outputDir.value)
//    println(eval)

    val testLabels = testData.flatMap(_.tokens).map(_.attr[CitationLabel])
    if (opts.useGrobidFeatures.value) {
      initGrobidFeatures(testData)
    }
    for (d <- testData) d.tokens.foreach(trainer.wordToFeatures)
    trainer.initSentenceFeatures(testData)
    testData.foreach{trainer.process}
//    val segEval = new SegmentEvaluation[CitationLabel]("(B|U)-", "(I|L)-", LabelDomain, testLabels.toIndexedSeq)
//    println("TEST")
//    println(segEval)
    val f0 = trainer.evaluator.printEvaluationSingle(testData, "TEST FINAL")

    //    println("\nafter:")
//    trainingData.head.tokens.take(10).foreach(t => println(s"${t.string} ${t.attr[CitationLabel].categoryValue}"))
//    CitationCRFTrainer.printDocument(trainingData.head)
    f0
  }
}

object TestCitationModel {
  URLHandlerSetup.poke()
  def main(args: Array[String]): Unit = {
    println(args.mkString(", "))
    val opts = new TrainCitationModelOptions
    opts.parse(args)
    opts.dataSet.value match {
      case "grobid" => processGrobid(opts)
      case _ => processDefault(opts)
    }
  }
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
    for (d <- docs) d.tokens.foreach(model.wordToFeatures)
    model.initSentenceFeatures(docs)
    docs.foreach(model.process)
    if (print) model.evaluator.printEvaluation(docs, docs, "FINAL")
  }
  def processDefault(opts: TrainCitationModelOptions): Unit = {
    val model = loadModel(opts.modelFile.value, opts.lexiconUrl.value)
    val testingData = LoadHier.fromFile(opts.testFile.value)
    process(testingData, model)
  }

  def processGrobid(opts: TrainCitationModelOptions): Unit = {
    val testingData = LoadGrobid.fromFilename(opts.testFile.value, withFeatures=opts.useGrobidFeatures.value)
//    println("before:")
//    CitationCRFTrainer.printDocument(testingData.head)
    val trainer = new CitationCRFTrainer
    trainer.deserialize(new URL(opts.modelFile.value).openStream())
    if (opts.useGrobidFeatures.value) {
      testingData.flatMap(_.tokens).foreach { token =>
        token.attr += new CitationFeatures(token)
        token.attr[CitationFeatures] ++= token.attr[PreFeatures].features
      }
    } else {
      val lexes = List("institution.lst", "tech.lst", "note.lst", "WikiLocations.lst", "WikiLocationsRedirects.lst", "WikiOrganizations.lst", "WikiOrganizationsRedirects.lst",
        "cardinalNumber.txt", "known_corporations.lst", "known_country.lst", "known_name.lst", "known_names.big.lst", "known_state.lst", "temporal_words.txt", "authors.lst",
        "journal.lst", "names.lst", "publishers.lst")
      trainer.lexicons = new Lexicons(opts.lexiconUrl.value, lexes)
      OverSegmenter.overSegment(testingData, trainer.lexicons.urlPrefix) //TODO don't think we need this here? -ks
      for (d <- testingData) d.tokens.foreach(trainer.wordToFeatures)
      trainer.initSentenceFeatures(testingData)
    }
    println(s"feature domain size: ${CitationFeaturesDomain.dimensionDomain.size}")
    val tot = trainer.model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat
    val len = trainer.model.parameters.tensors.sumInts(_.length)
    val sparsity = tot / len
    println(s"model sparsity: $sparsity")
    import scala.collection.mutable.HashMap
    case class EvalThing(label: String) {
      var tp = 0; var fp = 0; var tn = 0; var fn = 0; var expected = 0
      def precision: Double = if (tp + fp == 0) 0.0 else 1.0 * tp / (tp + fp)
      def recall: Double = if (tp + fn == 0) 0.0 else 1.0 * tp / (tp + fn)
      def f1: Double = {
        val p = precision; val r = recall
        if (p + r == 0.0) 0.0 else (2.0 * p * r) / (p + r)
      }
      override def toString: String = s"$label f1=$f1 tp=$tp fp=$fp fn=$fn expected=$expected"

    }
    val table = new HashMap[String, EvalThing]()
    def baseLabel(s: String): String = if (s != "O") s.substring(2) else "O"
    testingData.foreach { doc =>
      trainer.process(doc)
      doc.tokens.foreach { t =>
        val guess = baseLabel(t.attr[CitationLabel].categoryValue)
        val gold = baseLabel(t.attr[CitationLabel].target.categoryValue)
        if (!table.contains(guess)) table(guess) = new EvalThing(guess)
        if (!table.contains(gold)) table(gold) = new EvalThing(gold)
        if (guess == gold) {
          table(gold).tp += 1
        } else {
          table(gold).fn += 1
          table(guess).fp += 1
        }
        table(gold).expected += 1
      }
    }
    println("=== MY EVALUATION ===")
    table.foreach { case (label, eval) => println(eval.toString) }
    println("")
    trainer.evaluator.printEvaluation(testingData, testingData, "FINAL")
    println("")
    val evaluator = new ExactlyLikeGrobidEvaluator(opts.rootDir.value)
    val (_, eval) = evaluator.evaluate(testingData, opts.outputFile.value, writeFiles=opts.writeEvals.value, outputDir=opts.outputDir.value)
    println(eval)
  }
}

object OptimizeCitationModel {
  def main(args: Array[String]): Unit = {
    println(args.mkString(", "))
    val opts = new TrainCitationModelOptions
    opts.parse(args)
    opts.saveModel.setValue(false)
    opts.writeEvals.setValue(false)
    val l1 = HyperParameter(opts.l1, new LogUniformDoubleSampler(1e-6, 1))
    val l2 = HyperParameter(opts.l2, new LogUniformDoubleSampler(1e-6, 1))
    val qs = new QSubExecutor(10, "edu.umass.cs.iesl.bibie.TrainCitationModel")
    val optimizer = new HyperParameterSearcher(opts, Seq(l1, l2), qs.execute, 200, 180, 60)
    val result = optimizer.optimize()
    println("Got results: " + result.mkString(" "))
    println("Best l1: " + opts.l1.value + " best l2: " + opts.l2.value)
    println("Running best configuration...")
    opts.saveModel.setValue(true)
    opts.writeEvals.setValue(true)
    import scala.concurrent.duration._
    import scala.concurrent.Await
    Await.result(qs.execute(opts.values.flatMap(_.unParse).toArray), 1.hours)
    println("Done.")
  }
}

class Hyperparams(opts: TrainCitationModelOptions) {
  val optimizer = opts.optimizer.value
  val rate = opts.rate.value
  val delta = opts.delta.value
  val l1 = opts.l1.value
  val l2 = opts.l2.value
  val root = opts.rootDir.value //not a hyperparameter but used for misc stuff
}

class TrainCitationModelOptions extends cc.factorie.util.DefaultCmdOptions with cc.factorie.app.nlp.SharedNLPCmdOptions {
  val trainFile = new CmdOption("train-file", "", "STRING", "UMass formatted training file.")
  val trainDir = new CmdOption("train-dir", "", "STRING", "path to directory of training files")
  val testFile = new CmdOption("test-file", "", "STRING", "UMass formatted testing file (or blank).")
  val testDir = new CmdOption("test-dir", "", "STRING", "path to directory of testing files")
  /* misc infrastructure */
  val evalFile = new CmdOption("eval", "", "STRING", "filename to which to write evaluation")
  // TODO rename to e.g. "tmpoutput" or something, name is misleading
  val outputFile = new CmdOption("output", "", "STRING", "output filename")
  val outputDir = new CmdOption("output-dir", "", "STRING", "directory to write evaluations to")
  val writeEvals = new CmdOption("write-evals", false, "BOOLEAN", "write evaluations to separate files?")
  val modelFile = new CmdOption("model-file", "citationCRF.factorie", "STRING", "File for saving model.")
  val lexiconUrl = new CmdOption("lexicons", "classpath:lexicons", "STRING", "URL prefix for lexicon files/resources named cities, companies, companysuffix, countries, days, firstname.high, ...")
  val saveModel = new CmdOption("save-model", true, "BOOLEAN", "Whether to save the model or just train/test.")
  val rootDir = new CmdOption("root-dir", "", "STRING", "path to root directory of project (used for various required IO operations)")
  val dataSet = new CmdOption("data-set", "umass-citation", "STRING", "umass-citation|grobid")
  val useGrobidFeatures = new CmdOption("use-grobid-features", false, "BOOLEAN", "use grobid features?")
  /* hyperparameters */
  val optimizer = new CmdOption("optimizer", "lbfgs", "STRING", "lbfgs|adagrad")
  val rate = new CmdOption("adagrad-rate", 0.35548827391837345, "FLOAT", "Adagrad learning rate.")
  val delta = new CmdOption("adagrad-delta", 1.9033917145173614E-6, "FLOAT", "Adagrad delta (ridge).")
  val l1 = new CmdOption("l1", 0.1, "FLOAT", "l1 regularizer strength")
  val l2 = new CmdOption("l2", 0.1, "FLOAT", "l2 regularizer strength")
}

class TestCitationModelOptions extends cc.factorie.util.DefaultCmdOptions with cc.factorie.app.nlp.SharedNLPCmdOptions {
  val testFile = new CmdOption("test-file", "", "STRING", "UMass formatted testing file.")
  val modelUrl = new CmdOption("model-url", "file://citationCRF.factorie", "STRING", "URL for loading model.")
  val lexiconUrl = new CmdOption("lexicons", "classpath:lexicons", "STRING", "URL prefix for lexicon files/prefixes named cities, companies, companysuffix, countries, days, firstname.high, ...")
}

// TODO I'm not sure why we actually need this? --ks
object URLHandlerSetup {
  System.setProperty("java.protocol.handler.pkgs", "bibie.protocols")
  // make sure this gets instantiated
  def poke(): Unit = {
    println("attaching resource url handler.")
  }
}





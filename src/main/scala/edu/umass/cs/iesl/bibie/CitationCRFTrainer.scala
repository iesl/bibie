package edu.umass.cs.iesl.bibie

import java.io._
import java.net.URL

import cc.factorie._
import cc.factorie.app.chain.SegmentEvaluation
import cc.factorie.app.nlp.{Sentence, TokenSpan, Document, Token}
import cc.factorie.app.strings._
import cc.factorie.la.DenseTensor2
import cc.factorie.model.DotTemplateWithStatistics2
import cc.factorie.optimize.{L2Regularization, LBFGS, LikelihoodExample, ThreadLocalBatchTrainer, AdaGradRDA, Trainer}
import cc.factorie.util._
import cc.factorie.variable._

object CitationLabelDomain extends CategoricalDomain[String]
abstract class CLabel(labelname: String) extends LabeledCategoricalVariable(labelname)
class CitationLabel(labelname: String, val token: Token) extends CLabel(labelname) {
  def domain = CitationLabelDomain
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


class CitationCRFTrainer(val lexicons: Lexicons) {

  /* Deserialize this CitationCRFTagger from the model at the given URL */
  def this(url:java.net.URL, lexicons: Lexicons) = {
    this(lexicons)
    if (url != null) {
      deserialize(url.openConnection.getInputStream)
      println("Found model")
    }
    else {
      println("model not found")
    }
  }

  /* Deserialize this CitationCRFTagger from the model at the given path on disk */
  def this(modelPath: String, lexicons: Lexicons) = {
    this(new URL("file://" + modelPath), lexicons)
  }

  val evaluator = new SegmentationEvaluation[CitationLabel](CitationLabelDomain)
  val model = new CitationCRFModel
  var loc = 0
  
  // TODO: add some global features to baseline CRF - like "containsthesis"
  def wordToFeatures(token: Token) {
    val features = new CitationFeatures(token)
    val preFeats = token.attr[PreFeatures]
    if(preFeats != null) features ++= preFeats.features
    val docSpans = token.document.attr[CitationSpanList].spans
    val word = token.string
    val email = """([\w\d\-\_]+)(\+\d+)?@([\w\d\-\.]+)"""
    val url = """((http|ftp|https):\/\/)?[\w\-_]+(\.[\w\-_]+)+([\w\-\.,@?^=%&amp;:/~\+#]*[\w\-\@?^=%&amp;/~\+#])?"""
    val lower = word.toLowerCase
    val replace = lower.replaceAll("\\.|,|\\)", "")
    features += "W=" + word
    features += "SIMPLE=" + simplifyDigits(word)
    features += "SIMPLE_LOWER=" + simplifyDigits(word).toLowerCase
    if (word.length > 3) features += "PRE=" + word.substring(0, 3)
    if (word.matches(Capitalized)) features += "CAPITALIZED"
    if (word.matches(AllCaps)) features += "ALLCAPS"
    if (word.matches(Numeric)) features += "NUMERIC"
    if (word.matches(ParenNumeric)) features += "PARENNUMERIC"
    if (word.matches(Punctuation)) features += "PUNCTUATION"
    if (ContainsDigit.findFirstMatchIn(word) != None) features += "CONTAINSDIGIT"
    if (word.contains(".")) features += "CONTAINSDOTS"
    if (word.contains("-")) features += "CONTAINSDASH"
    if (word.matches("[0-9]+\\-[0-9]+")) features += "POSSIBLEPAGES"
    if (word.matches("[A-Z]")) features += "CAPLETTER"
    if (word.matches("[a-zA-Z]")) features += "SINGLECHAR"
    if (word.matches("[A-Z]\\.")) features += "LONLEYINITIAL"
    if (word.matches(email)) features += "EMAIL"
    if (word.matches(url)) features += "URL"
    if (word.matches(EndComma)) features += "ENDCOMMA"
    if (word.matches(EndPeriod)) features += "ENDPERIOD"
    if (word.matches(EndFullColon)) features += "ENDFULLCOLON"
    if (word.matches(HasOpenParen)) features += "OPENPAREN"
    if (word.matches(HasClosedParen)) features += "CLOSEDPAREN"
    if (word.matches(HasOpenParen) && word.matches(HasClosedParen)) features += "OPENANDSHUT"
    if (word.matches(HasOpenSquare)) features += "OPENSQUARE"
    if (word.matches(HasClosedSquare)) features += "CLOSEDSQUARE"
    if (word.matches(".*[0-9]$")) features += "LASTNUM"
    if (word.matches(".*[A-Z]$")) features += "LASTUPPER"
    if (word.matches(".*[a-z]$")) features += "LASTLOWER"
    if (word.matches(".*\"$")) features += "ENDQUOTE"
    if (word.matches(".*\".$")) features += "QUOTEATEND"
    if (word.matches(".*;$")) features += "ENDSEMI"
    if (word.matches("^\".*")) features += "BEGINQUOTE"
    if (word.matches(".*\\\\")) features += "ENDFORWARD"
    if (word.matches("^\\\\.*")) features += "BEGINFORWARD"
    if (word.matches(".*,\"$")) features += "ENDCOMMAQUOTE"
    if (word.matches("^[\\'`].*")) features += "STARTSINGLEQUOTE"
    if (word.matches(".*'.?$")) features += "ENDSINGLEQUOTE"
    if (word.trim.toLowerCase == "and" || word.trim.toLowerCase == "&") features += "ISAND"
    if (word.matches(".*[1-9](th|nd|st).*")) features += "EVENTITERATION"
    if (word.trim.toLowerCase == "thesis" || word.trim.toLowerCase == "dissertation") features += "ISTHESIS"
    val counts = count(word)
    features += "NUMDIGITS=" + counts._1
    features += "NUMALPHA=" + counts._2
    features += "NUMDIGITS=" + counts._1 + "ALPHS=" + counts._2
    if (replace.matches(Numeric) && counts._1 == 4 && replace.toInt < 1900) features += "BEFORE1900"
    if (replace.matches(Numeric) && counts._1 == 4 && replace.toInt >= 1900) features += "AFTER1900"
    //if (word.matches(ParenNumeric) && counts._1 == 4 && word.replaceFirst("\\).?$",")").replaceAll("[\\)\\(]","").toInt < 1900) features += "BEFORE1900"
    //if (word.matches(ParenNumeric) && counts._1 == 4 && word.replaceFirst("\\).?$",")").replaceAll("[\\)\\(]","").toInt >= 1900) features += "AFTER1900"
    if (lower.startsWith("appeared") || lower.startsWith("submitted") || lower.startsWith("appear")) features += "STATUS"
    //if(token.startsSpansOfClass[SegmentSpan].nonEmpty) features += "PROBABLESEGMENT"
    if (docSpans.exists(span => span.tokens.head == token)) features += "PROBABLESEGMENT"
    if (lower.matches("(ed\\.|eds\\.|editor|editors).*")) features += "EDITOR"
    if (lower.matches("(proc\\.?|proceedings|trans\\.?|conf\\.?|symp\\.?|conference|symposium|workshop).*")) features += "BOOKTITLE"
    if (lower.matches("(university|dept\\.|department).*")) features += "INST"
    if (lower.matches("^p(p|ages|pps|gs)?\\.?")) features += "ISPAGES"
    if (lower.matches("(v\\.?|volume|vol\\.?).*")) features += "VOLUME"
    loc += 1
    token.attr += features
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

  // todo remove for loops
  def initSentenceFeatures(d: Document) {
      val docLength = d.tokens.size
      for (token <- d.tokens) {
        val per = token.position.toDouble / docLength.toDouble
        val binned = ((per * 60.0) / 5.0).floor
        token.attr[CitationFeatures] += "BIN=" + binned
      }

      for (t <- d.tokens) {
        if (t.nextWindow(10).count(_.string.toLowerCase.matches(".*(ed\\.|editor|eds|editors).*")) > 0) t.attr[CitationFeatures] += "POSSIBLEEDITOR"
      }

      for (s <- d.sentences) {
        for (token <- s.tokens) {
          for (lexicon <- lexicons(token)) {
            token.attr[CitationFeatures] += "LEX=" + lexicon
          }
        }
        cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(s.tokens, (t: Token) => t.attr[CitationFeatures], "^[^@]*$", List(0, 0), List(1), List(2), List(-1), List(-2))
      }
  }

  // todo remove for loops
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
  }

  def train(trainDocuments: Seq[Document], testDocuments: Seq[Document], params: Hyperparams)(implicit random: scala.util.Random): Double = {

    // generate features
    computeDocumentsFeatures(trainDocuments)
    println(s"feature domain size: ${CitationFeaturesDomain.dimensionDomain.size}")
    CitationFeaturesDomain.freeze()
    computeDocumentsFeatures(testDocuments)

    // todo feature count cutoff?

    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels: Seq[CitationLabel] = trainDocuments.flatMap(_.tokens).map(_.attr[CitationLabel])
    val testLabels: Seq[CitationLabel] = testDocuments.flatMap(_.tokens).map(_.attr[CitationLabel])
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
        Trainer.onlineTrain(model.parameters, examples, evaluate=evaluate, useParallelTrainer=false, maxIterations=params.numIterations, optimizer=optimizer)
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

  /* Assumes features are already computed */
  def predict(document: Document): Document = {
    for (sentence <- document.sentences if sentence.tokens.size > 0) {
      val vars = sentence.tokens.map(_.attr[CitationLabel]).toSeq
      val sum = CitationBIOHelper.infer(vars, model)
      sum.setToMaximize(null)
    }
    document
  }

  def computeDocumentsFeatures(documents: Seq[Document]) = documents.foreach{computeDocumentFeatures}

  def computeDocumentFeatures(document: Document) = {
    for (sentence <- document.sentences if sentence.tokens.size > 0) computeTokenFeatures(sentence)
    initSentenceFeatures(document)
  }

  def computeTokenFeatures(sentence: Sentence) = {
    /* Elaborate process of computing features */
    sentence.tokens.foreach { token =>
      token.attr += new CitationFeatures(token)
      val preFeats = token.attr[PreFeatures]
      if (preFeats != null) token.attr[CitationFeatures] ++= preFeats.features
      wordToFeatures(token)
    }
  }

  def process(document: Document): Document = {
    if (document.tokens.size == 0) return document
    // todo this requires iterating over the tokens twice but may be unavoidable if accuracy is improved?
    computeDocumentFeatures(document)
    predict(document)
  }

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataOutputStream(stream)
    BinarySerializer.serialize(CitationLabelDomain, is)
    BinarySerializer.serialize(CitationFeaturesDomain.dimensionDomain, is)
    BinarySerializer.serialize(model, is)
    is.close()
  }
  def deserialize(stream: InputStream): Unit = deserialize(model, stream)

  def deserialize(theModel: CitationCRFModel, stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(stream)
    BinarySerializer.deserialize(CitationLabelDomain, is)
    CitationLabelDomain.freeze()
    println(s"Deserialized CitationLabelDomain: ${CitationLabelDomain.dimensionSize}")
    BinarySerializer.deserialize(CitationFeaturesDomain.dimensionDomain, is)
    CitationFeaturesDomain.freeze()
    println(s"Deserialized CitationFeaturesDomain: ${CitationFeaturesDomain.dimensionSize}")

    BinarySerializer.deserialize(theModel, is)
    println(s"Deserialized model: ${theModel.sparsity}")
    is.close()
  }
}

// TODO: implement DocumentAnnotator to add citations
object TrainCitationModel extends HyperparameterMain {

  def evaluateParameters(args: Array[String]): Double = {
    println(args.mkString(", "))
    val opts = new TrainCitationModelOptions
    opts.parse(args)
    val params = new Hyperparams(opts)

    val lexiconDir = opts.lexiconUrl.value
    val tagger = new CitationCRFTrainer(new DefaultLexicons(lexiconDir))

    val allTrainingData = opts.dataset.value match {
      case "grobid" => LoadGrobid.fromFilename(opts.trainFile.value, withFeatures=opts.useGrobidFeatures.value)
      case _ => LoadHier.fromFile(opts.trainFile.value)
    }

    val trainPortion = (allTrainingData.length.toDouble * opts.trainPortion.value).floor.toInt
    val devPortion = (allTrainingData.length.toDouble * opts.testPortion.value).floor.toInt
    val trainingData = allTrainingData.take(trainPortion)
    val devData = allTrainingData.drop(trainPortion).take(devPortion)

    println("Training: " + trainingData.size + " Dev: " + devData.size)

    OverSegmenter.overSegment(trainingData ++ devData, lexiconDir) // todo do this in loader?

    val f0 = tagger.train(trainingData, devData, params)

    if (opts.saveModel.value) tagger.serialize(new FileOutputStream(opts.modelFile.value))

    if(opts.testFile.wasInvoked) TestCitationModel.main(args)

    f0
  }
}

object TestCitationModel {
  def main(args: Array[String]): Unit = {
    println(args.mkString(", "))
    val opts = new TrainCitationModelOptions
    opts.parse(args)
    val params = new Hyperparams(opts)

    val lexiconDir = opts.lexiconUrl.value

    val testData = opts.dataset.value match {
      case "grobid" => LoadGrobid.fromFilename(opts.testFile.value, withFeatures = opts.useGrobidFeatures.value)
      case _ => LoadHier.fromFile(opts.testFile.value)
    }

    val tagger = new CitationCRFTrainer(opts.modelFile.value, new DefaultLexicons(lexiconDir))
    OverSegmenter.overSegment(testData, lexiconDir)
    testData.foreach{tagger.process}
    tagger.evaluator.printEvaluationSingle(testData, "TEST FINAL")

    if (opts.outputTagged.wasInvoked) {
      val outputFname = opts.outputTagged.value
      println(s"writing tagged output to $outputFname")
      val writer = new PrintWriter(outputFname, "utf-8")
      testData.foreach { doc => {
        doc.tokens.foreach { token => {
          val label = token.attr[CitationLabel]
          writer.println(s"${token.string}\t${label.target.categoryValue}\t${label.categoryValue}")
        }}
        writer.println()
      }}
      writer.close()
    }
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
    val rate = HyperParameter(opts.rate, new LogUniformDoubleSampler(1e-4, 1))
    val delta = HyperParameter(opts.delta, new LogUniformDoubleSampler(1e-4, 1))
    val qs = new QSubExecutor(10, "edu.umass.cs.iesl.bibie.TrainCitationModel")
    val optimizer = new HyperParameterSearcher(opts, Seq(l1, l2, rate, delta), qs.execute, 200, 180, 60)
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
  val numIterations = opts.numIterations.value
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
  val outputTagged = new CmdOption("output-tagged", "", "STRING", "tagged file output filename")
  val writeEvals = new CmdOption("write-evals", false, "BOOLEAN", "write evaluations to separate files?")
  val modelFile = new CmdOption("model-file", "citationCRF.factorie", "STRING", "File for saving model.")
  val lexiconUrl = new CmdOption("lexicons", "classpath:lexicons", "STRING", "URL prefix for lexicon files/resources named cities, companies, companysuffix, countries, days, firstname.high, ...")
  val saveModel = new CmdOption("save-model", true, "BOOLEAN", "Whether to save the model or just train/test.")
  val rootDir = new CmdOption("root-dir", "", "STRING", "path to root directory of project (used for various required IO operations)")
  val dataset = new CmdOption("dataset", "umass-citation", "STRING", "umass-citation|grobid")
  val useGrobidFeatures = new CmdOption("use-grobid-features", false, "BOOLEAN", "use grobid features?")
  /* hyperparameters */
  val optimizer = new CmdOption("optimizer", "lbfgs", "STRING", "lbfgs|adagrad")
  val rate = new CmdOption("adagrad-rate", 0.35548827391837345, "FLOAT", "Adagrad learning rate.")
  val delta = new CmdOption("adagrad-delta", 1.9033917145173614E-6, "FLOAT", "Adagrad delta (ridge).")
  val l1 = new CmdOption("l1", 0.1, "FLOAT", "l1 regularizer strength")
  val l2 = new CmdOption("l2", 0.1, "FLOAT", "l2 regularizer strength")
  val numIterations = new CmdOption("num-iterations", 5, "INT", "Number of training iterations")
}

class TestCitationModelOptions extends cc.factorie.util.DefaultCmdOptions with cc.factorie.app.nlp.SharedNLPCmdOptions {
  val testFile = new CmdOption("test-file", "", "STRING", "UMass formatted testing file.")
  val modelUrl = new CmdOption("model-url", "file://citationCRF.factorie", "STRING", "URL for loading model.")
  val lexiconUrl = new CmdOption("lexicons", "classpath:lexicons", "STRING", "URL prefix for lexicon files/prefixes named cities, companies, companysuffix, countries, days, firstname.high, ...")
}




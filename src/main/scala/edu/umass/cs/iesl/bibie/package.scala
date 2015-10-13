package edu.umass.cs.iesl

import cc.factorie.util._
import cc.factorie.app.nlp._
import edu.umass.cs.iesl.bibie.segment.CitationSpanList
import edu.umass.cs.iesl.bibie.model.CitationSpan

/**
 * Created by strubell on 7/3/15.
 */
package object bibie {
  // seed rng with constant for repeatability
  implicit val random = new scala.util.Random(0)

  /* data set id's */
  final val DATA_GROBID = "grobid"
  final val DATA_UMASS = "umass-citation"

  /* feature set id's */
  final val FEATURES_GROBID = "grobid"
  final val FEATURES_UMASS = "umass"
  final val FEATURES_BOTH = "both"

  class BibieOptions extends DefaultCmdOptions with cc.factorie.app.nlp.SharedNLPCmdOptions {
    val trainFile = new CmdOption("train-file", "", "STRING", "UMass formatted training file.")
    val devFile = new CmdOption("dev-file", "", "STRING", "UMass formatted dev file.")
    val testFile = new CmdOption("test-file", "", "STRING", "UMass formatted test file.")
    val rootPath = new CmdOption[String]("root-path", "", "STRING", "path to directory where you want to save things")
    val saveModel = new CmdOption[Boolean]("save-model", true, "BOOLEAN", "whether or not to save the model")
    val modelFile = new CmdOption[String]("model-file", "bibie.factorie", "STRING", "file to save model to or load model from")
    val lexiconUrl = new CmdOption("lexicons", "classpath:lexicons", "STRING", "path to lexicon files")

    val taggerType = new CmdOption[String]("tagger-type", "default", "STRING", "tagger type: grobid|default|combined")

    /* experiment id */
//    val dataSet = new CmdOption[String]("data-set", "", "STRING", "which data set to use: grobid|umass-citation")
//    val featureSet = new CmdOption[String]("feature-set", "", "STRING", "which feature set to use: grobid|umass|both (note: if dataSet==umass-citation, umass features will be used)")

    /* training hyperparameters */
    val useCrossValidation = new CmdOption[Boolean]("use-cross-validation", false, "BOOLEAN", "use cross validation")
    val nFolds = new CmdOption[Int]("n-folds", 5, "INT", "# folds to use in cross validation")
    val optimizer = new CmdOption[String]("optimizer", "lbfgs", "STRING", "lbfgs|adagrad")
    val rate = new CmdOption("adagrad-rate", 0.35548827391837345, "FLOAT", "Adagrad learning rate.")
    val delta = new CmdOption("adagrad-delta", 1.9033917145173614E-6, "FLOAT", "Adagrad delta (ridge).")
    val l1 = new CmdOption("l1", 0.1, "FLOAT", "l1 regularizer strength")
    val l2 = new CmdOption("l2", 0.1, "FLOAT", "l2 regularizer strength")
    val numIterations = new CmdOption("num-iterations", 5, "INT", "Number of training iterations")
  }

  implicit class DocumentExtras(doc: Document) {
    def toXML: String = {
      val spans: Seq[CitationSpan] = doc.attr[CitationSpanList].spans
      val xml = spans.map(_.toXML)
      var str = s"<document>\n<name>${doc.name}</name>\n"
      str += xml.mkString("\n")
      str += "\n</document>"
      str
    }
  }

}

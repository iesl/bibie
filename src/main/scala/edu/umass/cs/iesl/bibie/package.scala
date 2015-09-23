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

  class BibieOptions extends DefaultCmdOptions with cc.factorie.app.nlp.SharedNLPCmdOptions {
    val trainFile = new CmdOption("train-file", "", "STRING", "UMass formatted training file.")
    val devFile = new CmdOption("dev-file", "", "STRING", "UMass formatted dev file.")
    val testFile = new CmdOption("test-file", "", "STRING", "UMass formatted test file.")
    val saveModel = new CmdOption[Boolean]("save-model", true, "BOOLEAN", "whether or not to save the model")
    val modelFile = new CmdOption[String]("model-file", "bibie.factorie", "STRING", "file to save model to or load model from")
    val lexiconUrl = new CmdOption("lexicons", "classpath:lexicons", "STRING", "path to lexicon files")

    /* training hyperparameters */
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

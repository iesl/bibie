package edu.umass.cs.iesl.bibie

import cc.factorie.util._

import scala.io.StdIn

/**
 * @author Kate Silverstein 
 *         created on 9/12/15
 */
object Cli {

  class BibieOpts extends DefaultCmdOptions {
    val model = new CmdOption[String]("model", "", "STRING", "model")
    val inputDir = new CmdOption[String]("input-dir", "", "STRING", "input dir")
  }

  val opts = new BibieOpts

  def main(args: Array[String]): Unit = {
    opts.parse(args)



  }

}

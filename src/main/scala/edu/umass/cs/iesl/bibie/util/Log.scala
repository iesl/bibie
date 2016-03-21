package edu.umass.cs.iesl.bibie.util

import java.util.logging.{FileHandler, Logger, SimpleFormatter}

/**
 * Created by kate on 1/26/16.
 */

class Log(filename: String) {
  val log = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME)
  init()
  def init(): Unit = {
    val handlers = log.getHandlers
    val fh = new FileHandler(filename)
    val fmt = new SimpleFormatter()
    fh.setFormatter(fmt)
    log.addHandler(fh)
  }
}
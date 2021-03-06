package edu.umass.cs.iesl.bibie.model

import edu.umass.cs.iesl.bibie._

/**
 * @author Kate Silverstein 
 *         created on 9/13/15
 */
class Hyperparams(opts: BibieOptions) {
  val lexiconUrl = opts.lexiconUrl.value
  val optimizer = opts.optimizer.value
  val learningRate = opts.rate.value
  val delta = opts.delta.value
  val l1 = opts.l1.value
  val l2 = opts.l2.value
  val numIterations = opts.numIterations.value
  val trimBelow: Int = opts.trimBelow.value
  val segmentScheme: String = opts.segmentScheme.value
}

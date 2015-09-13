package edu.umass.cs.iesl.bibie.model

/**
 * @author Kate Silverstein 
 *         created on 9/13/15
 */
class Hyperparams(opts: TrainCitationModelOptions) {
  val optimizer = opts.optimizer.value
  val rate = opts.rate.value
  val delta = opts.delta.value
  val l1 = opts.l1.value
  val l2 = opts.l2.value
  val numIterations = opts.numIterations.value
}

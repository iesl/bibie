package edu.umass.cs.iesl

/**
 * Created by strubell on 7/3/15.
 */
package object bibie {
  // seed rng with constant for repeatability
  implicit val random = new scala.util.Random(0)
}

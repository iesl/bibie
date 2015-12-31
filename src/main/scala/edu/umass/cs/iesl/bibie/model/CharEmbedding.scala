package edu.umass.cs.iesl.bibie.model

import scala.io.Source
/**
 * Created by kate on 12/28/15.
 *
 */

/**
 * Simple container for PRE-TRAINED character embeddings
 * @param inputFile each line corresponds to the embedding of a single character
 * @param vocabFile file containing a map from character -> vocab index
 * @param embeddingDim for debugging purposes
 */
case class CharEmbedding(inputFile: String, vocabFile: String, embeddingDim: Int) {
  val vmap: Map[Char,Int] = Source.fromFile(vocabFile).getLines().toSeq.filter(line => line.split(" ").length == 2).map { line =>
    val parts = line.split(" ")
    val char: Char = {
      val head = parts.head
      if (head.equals("\\n")) '\n'
      else if (head.equals("\\t")) '\t'
      else parts.head.charAt(0)
    }
    val idx: Int = Integer.parseInt(parts.last.trim)
    char -> idx
  }.toMap
  println(s"vmap: ${vmap.size}")
  vmap.foreach { case (k, v) => println(s"$k $v")}
  val embeddings: Array[Array[Double]] = Source.fromFile(inputFile).getLines().map { line =>
    val parts = line.trim.split(" ")
    assert(parts.length == embeddingDim, s"line len ${parts.length} neq embedding dim $embeddingDim")
    parts.map(value => value.toDouble)
  }.toArray
  def lookup(c: Char): Option[Array[Double]] = {
    val idx = vmap.get(c)
    idx match {
      case Some(index) => Some(embeddings(index))
      case None => None
    }
  }
}

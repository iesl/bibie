package edu.umass.cs.iesl.bibie.load

import cc.factorie.app.nlp.{Document, Sentence, Token}
import edu.umass.cs.iesl.bibie.model.{CitationLabelDomain, CitationLabel}

import edu.umass.cs.iesl.bibie.segment._

import scala.collection.mutable
import scala.io.Source

import java.util.logging.Logger

object LoadHier {

  private val logger = Logger.getLogger(getClass.getName)

  def fromFile(filename: String): Seq[Document] = {

    logger.info("loading filename: " + filename)

    var document: Document = null
    val documents = new collection.mutable.ArrayBuffer[Document]
    var sentence: Sentence = null

    for (l <- Source.fromFile(filename).getLines()) {
      document = new Document("").setName("NLP-" + documents.length)
      documents += document
      sentence = new Sentence(document)
      val tags = new mutable.HashMap[String, (Int, Int)]()
      var count = 0
      for (t <- l.split("\\s+")) {
        count += 1
        if (t.matches("<[a-z\\-_]*>")) {tags(t.replaceAll("<|>", "")) = (count, 0)}
        else if (t.matches("</[a-z\\-_]*>")) tags.remove(t.replaceAll("<|>|/", ""))
        else {

          // TODO fixme base model is trained on "" not "O"
          val tag = /*if (tags.isEmpty) "O" else */ tags.toSeq.sortBy(_._2._1).map(ta => (if (ta._2._2 == 0) "B-" else "I-") + ta._1).mkString(":")
          if (Exclude.ar.contains(tag)) {
            println("problem with" + tag)
            println("on token" + t)
            println("Here: " + sentence.tokens.map(_.string).mkString(" "))
            println("===========")
          }
          /*if(tag == "I-venue") {
            println("Empty tag: " + tag)
            println("on token" + t)
            println("Here: " + sentence.tokens.map(_.string).mkString(" "))
            println("===========")
          }*/
          if (sentence.length > 0) document.appendString(" ")
          val token = new Token(sentence, t)
          token.attr += new CitationLabel(if (!CitationLabelDomain.frozen || CitationLabelDomain.categories.contains(tag)) tag else "", token)
          tags.foreach {t => tags(t._1) = (t._2._1, t._2._2 + 1)}
        }
      }
    }
    println("Loaded: " + documents.size + " number of citations.")
    documents.toSeq
  }

  def main(args: Array[String]) {
    for (d <- fromFile(args.head)) {
      d.tokens.foreach { token => {
        val label = token.attr[CitationLabel]
        println(s"${token.string}\t${label.target.categoryValue}\t${label.categoryValue}")
      }}
    }
  }
}

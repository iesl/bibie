package edu.umass.cs.iesl.bibie

import cc.factorie.app.nlp.{Token, Sentence, Document}
import io.Source
import collection.mutable

object LoadHier {
  def fromFile(filename: String): Seq[Document] = {
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
          token.attr += new CitationLabel(if (!LabelDomain.frozen || LabelDomain.categories.contains(tag)) tag else "O", token)
          tags.foreach {t => tags(t._1) = (t._2._1, t._2._2 + 1)}
        }
      }
    }
    println("Loaded: " + documents.size + " number of citations.")
    documents.toSeq
  }

  def main(args: Array[String]) {
    for (d <- fromFile(args.head)) {
      CitationCRFTrainer.printDocument(d)
    }
  }
}
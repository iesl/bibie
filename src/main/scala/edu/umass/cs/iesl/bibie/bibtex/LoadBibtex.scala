package edu.umass.cs.iesl.bibie.bibtex

/**
 * Created by kate on 12/31/15.
 */

import cc.factorie.app.nlp.segment.DeterministicTokenizer
import cc.factorie.app.nlp.{Document, Sentence, Token, TokenSpan}
import java.io.File
import org.json4s._
import org.json4s.jackson.JsonMethods._



object LoadBibtex {
  def main(args: Array[String]): Unit = {
    val dir = args(0)
    val docs = fromDir(dir)
    println(docs.length)
    docs.take(10).foreach { doc =>
      println(s"doc: sents=${doc.sentenceCount}, toks=${doc.tokenCount}")
      doc.sentences.foreach { s =>
        println(s"\t${s.attr[BibtexLabel].target.categoryValue}")
        println(s"\t${s.tokens.map(_.string).mkString(" ")}")
      }
      println("")
    }
  }

  def fromFilename(filename: String): Document = {
    val doc = new Document("")
    val input = file2JsonInput(new File(filename))
    val json = parse(input)
    for (JObject(child) <- json) {
      child.foreach { case (key, JString(value)) =>
        if (value.length > 0) {
          val tmp = new Document(value)
          DeterministicTokenizer.process(tmp)
          val tokens = tmp.tokens.map { t => new Token(doc, t.string) }
          val span = new TokenSpan(tokens.toSeq)
          val sent = new Sentence(doc.asSection, span.start, span.length)
          sent.attr += new BibtexLabel(key, sent)
        }
      }
    }
    doc
  }

  def fromDir(dir: String): Seq[Document] = {
    val filenames = new File(dir).listFiles().map(_.getAbsolutePath)
    filenames.map(fromFilename).filter(_.tokenCount > 0)
  }

}

package edu.umass.cs.iesl.bibie.segment

import java.util.concurrent.ExecutorService
import java.util.logging.Logger

import cc.factorie.app.nlp.{Document, Token, DocumentAnnotator}
import edu.umass.cs.iesl.bibie.model.{CitationSpan, SegmentSpan}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.io.Source

// load from classpath
class OverSegmenter(locUrls: Seq[String] = Array[String](), monUrls: Seq[String] = Array[String]()) {
  //REGEXS//
  val locations = HashMap[String, Boolean]()
  val months = HashMap[String, Boolean]()

  val aggressive = false
  loadLocations()
  loadMonths()
  locations.remove("university")
  locations.remove("center")

  def loadLocations() {
    for (lf <- locUrls) {
      println("loading " + lf)
      for (line <- Source.fromURL(lf).getLines()) {
        if (line.toLowerCase.replaceAll("\\.|\\(|\\)", "").length > 1) {
          locations(line.toLowerCase.replaceAll("\\.|\\(|\\)", "")) = true
          for (loc <- line.split(","))
            locations(loc.toLowerCase.replaceAll("\\.|\\(|\\)", "")) = true
        }
      }
    }
  }

  def loadMonths(): Unit = {
    for (lf <- monUrls) {
      println("loading " + lf)
      for (line <- Source.fromURL(lf).getLines()) {
        if (line.length > 2) {
          months(line.toLowerCase.replaceAll("\\.|\\(|\\)|\\-", "")) = true
          locations.remove(line.toLowerCase.replaceAll("\\.|\\(|\\)|\\-", ""))
        }
      }
    }
  }

  val startsSeq = "(\\^|\\[|\"|``|\\\\\\\\).*".r
  // Matches some charactor seq that indicates some seq of tokens EX. ' `` [ { (

  abstract class State
  case class Unstarted(d: Int) extends State
  case class Begin(start: Token, opener: String = "") extends State
  case class End(begin: Begin) extends State
  case class Finished(end: End) extends State

  val ender = new mutable.HashMap[String, Array[String]]()
  ender("[") = Array("]")
  ender("\"") = Array("\"", "''")
  ender("\\\\") = Array("\"")
  ender("{") = Array("{")
  ender("``") = Array("\"", "''")
  //ender("'") = Array("'")
  ender("(") = Array(")")
  ender("^") = Array("~")

  var token: Token = null

  def next(): Unit =
    token = if (token.hasNext) token.next else null
  def prev(): Unit =
    token = if (token.hasPrev) token.prev else null

  //  def printSegmentation(document : Document) {
  //    segment(document) //.foreach( seg => if(seg.replaceAll(",","").trim.length() > 0) println(seg) )
  //    printDocument(document : Document)
  //  }

  //  def printDocument(document : Document) {
  //    for (token <- document.tokens) {
  //      token.startsSpansOfClass[CitationSpan].foreach(span => print("<"+span.label.value+">"))
  //      print(token.string)
  //      token.endsSpansOfClass[CitationSpan].foreach(span => print("</"+span.label.value+">"))
  //      print(" ")
  //    }
  //    println
  //  }

  def clean(token: Token): String = token.string.replaceAll("\\(|\\)|\\-", "").trim

  def isDigits(token: Token): Boolean = clean(token).matches("[A-Z]?[0-9]+[A-Z]?/?")

  def isYear(token: Token): Boolean =
    token.string.matches("[0-9]+") && clean(token).length == 4 && clean(token).toInt > 1900 && clean(token).toInt < 2025

  def isAbr(token: Token): Boolean = clean(token) != "Vol." && clean(token) != "Proc." && clean(token) != "No." && clean(token).matches("([A-Z][a-zA-Z]*\\.)|\\-|[A-Z][A-Z]?")

  def prevNotPagesOrAp(token: Token): Boolean = {
    if (token.hasPrev) {
      val string = token.prev.string.replaceAll("\\.", "").trim.toLowerCase
      var result = false
      result = string != "pp" && string != "#" && string != "ws" && string != "p" && string != "on" && string != "the" && string != "page" && string != "pgs" && string != "pages" && string != "'" && string != "vol" && string != "volume" && string != "no" && string != "nos" && string != "number"
      if (result && token.prev.hasPrev && token.prev.string == ".") {
        val pstring = token.prev.prev.string.replaceAll("\\.", "").trim.toLowerCase
        result = pstring != "vol" && pstring != "no" && pstring != "nos" && pstring != "p" && token.prev.prev.string != "pp" && pstring != "pgs" && pstring != "n"
      }
      result
    } else true
  }

  def noIndicator(token: Token): Boolean = {
    if (token.hasPrev) {
      val string = token.prev.string.replaceAll("\\.", "").trim.toLowerCase
      string != "vol" && string != "phys" && string != "phd" && string != "nos" && string != "univ" && string != "trans" && string != "int'l" && string != "comm" && string != "rec" && string != "pp" && string != "st" && string != "trans" && string != "dept" && string != "v" && string != "sympos" && string != "ann" && string != "nos" && string != "work" && string != "annu" && string != "no" && string != "proc" && string != "procs" && string != "n" && string != "int" && string != "inst" && string != "intl" && string != "symp" && string != "conf" && string != "int" && string != "p" && string != "pgs"
    } else true
  }

  def notColonExp(token: Token): Boolean = {
    if (token.hasPrev) {
      val string = token.prev.string.replaceAll("\\.", "").trim.toLowerCase
      string != "in" && string != "pages"
    } else true
  }

  def nextNot(token: Token): Boolean = {
    if (token.hasNext && aggressive) {
      val string = token.next.string.replaceAll("\\.|\\(|\\)", "").trim.toLowerCase
      string != "eds" && string != "editor" && string != "ed" && string != "editors"
    } else true
  }

  def notEditor(token: Token): Boolean = {
    if (token.hasNext) {
      val string = token.next.string.replaceAll("\\.|\\(|\\)", "").trim.toLowerCase
      string != "eds" && string != "editor" && string != "ed" && string != "editors"
    } else true
  }

  def canFindEd(token: Token): Boolean = {
    var search = token
    var canFind = false
    var count = 0
    while (search.hasNext && count < 10) {
      val string = search.string.replaceAll("\\.|\\(|\\)", "").trim.toLowerCase
      if (string == "of") return false
      if (string == "eds" || string == "editor" || string == "ed" || string == "editors") canFind = true
      if (!search.string.matches("[A-Z]\\.") && !search.string.matches("\\.|,|;|:")) count += 1
      search = search.next
    }
    canFind
  }

  def findEd(token: Token): Token = {
    var search = token
    var canFind = false
    while (search.hasNext && !canFind) {
      val string = search.string.replaceAll("\\.|\\(|\\)", "").trim.toLowerCase
      if (string == "eds" || string == "editor" || string == "ed" || string == "editors") canFind = true
      search = search.next
    }
    while (search.hasNext && search.next.string.matches("\\(|\\)|:|,")) search = search.next
    search
  }

  def noLocation(token: Token): Boolean = {
    var uni = false
    var search = token
    while (search.hasPrev && search.prev.string != ",") {
      search = search.prev
      if (search.string.contains("Uni")) uni = true
    }
    if (uni) return true
    if (token.hasPrev && !token.prev.string.matches("[A-Z][a-zA-Z\\.]*")) return true
    if (token.hasNext && token.hasPrev && token.prev.hasPrev && token.prev.prev.hasPrev && token.prev.prev.prev.hasPrev) {
      val p = token.prev.string.replaceAll("\\.|\\(|\\)", "").trim.toLowerCase
      val n = token.next.string.replaceAll("\\.|\\(|\\)", "").trim.toLowerCase
      val m = locations.contains(p)
      val mm = locations.contains(n)
      if (locations.contains(p) && locations.contains(n)) false
      else {
        if (token.prev.hasPrev) {
          val pp = token.prev.prev.string.replaceAll("\\.|\\(|\\)", "").trim.toLowerCase + " " + p
          val mpp = locations.contains(pp)
          val mppp = if (token.prev.prev.hasPrev) locations.contains(token.prev.prev.prev.string.replaceAll("\\.|\\(|\\)", "").trim.toLowerCase + " " + pp) else false
          val nn = if (token.next.hasNext) locations.contains(n + " " + token.next.next.string.replaceAll("\\.|\\(|\\)", "").trim.toLowerCase) else false
          val d = !((m || mpp || mppp) && (mm || nn))
          d
        } else true
      }
    } else true
  }

  def findLocation(token: Token): Token = {
    if (token.hasPrev && token.prev.hasPrev && token.prev.prev.hasPrev && token.hasNext) {
      val p = token.prev
      val pp = token.prev.prev
      val ppp = token.prev.prev.prev
      val ps = p.string.replaceAll("\\.|\\(|\\)", "").trim.toLowerCase
      val pps = pp.string.replaceAll("\\.|\\(|\\)", "").trim.toLowerCase + " " + ps
      val l = if (locations.contains(ps)) p
      else if (locations.contains(pps)) pp
      else ppp
      if (l.hasPrev && l.prev.string == "," && !noLocation(l.prev)) null.asInstanceOf[Token]
      else l
    } else null.asInstanceOf[Token]
  }

  def findEndLocation(token: Token): Token = {
    if (token.hasNext && token.next.hasNext && token.next.hasNext) {
      val n = token.next
      val nn = token.next.next
      val l = if (locations.contains(n.string.replaceAll("\\.|\\(|\\)", "").trim.toLowerCase)) n
      else nn
      if (l.hasNext && ((l.next.string == "," && !noLocation(l.next)) || !notZip(l.next))) null.asInstanceOf[Token]
      else l
    } else null.asInstanceOf[Token]
  }


  def noDate(token: Token): Boolean = {
    if (token.hasNext && token.hasPrev) {
      val p = token.prev.string.replaceAll("\\.|\\(|\\)", "").trim.toLowerCase
      val m = months.contains(p)
      val d = isDigits(token.next)
      !((months.contains(p) && isYear(token.next)) || (isYear(token) && months.contains(token.next.string.replaceAll("\\.|\\(|\\)", "").trim.toLowerCase)))
    } else true
  }

  def prevNotDate(token: Token): Boolean = {
    if (token.hasPrev) {
      val p = token.prev.string.replaceAll("\\.|\\(|\\)", "").trim.toLowerCase
      if (p.split("\\-").size > 0) !months.contains(p.split("\\-").head) else !months.contains(p)
    } else true
  }

  def notHyphen(token: Token): Boolean = {
    if (token.hasNext && token.next.hasNext) {
      val n = token.next.string
      val nn = token.next.next.string
      n != "-" && !nn.matches("[A-Z]\\.")
    } else true
  }

  def hasHyphen(token: Token): Boolean = {
    if (token.hasPrev) {
      val n = token.prev.string
      n != "-"
    } else true
  }

  def notAndExclude(token: Token): Boolean = {
    if (token.hasNext) {
      val n = token.next.string
      n != "the"
    } else true
  }

  def notAuthorCheck(sp: CitationSpan): Boolean = {
    var hasAnd = false
    var count = 0
    for (t <- sp.tokens) {
      if (t.string == "and" || t.string == "&") hasAnd = true
      if ((t.string == "." || t.string == ",") && count <= 3) count = 0
      if (t.string.length > 1) count += 1
    }
    count > 3 || hasAnd
  }

  def notAuthorNoAndCheck(sp: CitationSpan): Boolean = {
    var hasAnd = false
    var count = 0
    for (t <- sp.tokens) {
      if ((t.string == "." || t.string == ",") && count <= 3) count = 0
      if (t.string.length > 1) count += 1
    }
    count > 3
  }


  def notMiddleDate(token: Token): Boolean = {
    if (token.hasNext && token.hasPrev && token.prev.hasPrev && token.prev.prev.hasPrev && token.prev.prev.prev.hasPrev) {
      val t = token.next
      val p = token.prev
      val pp = token.prev.prev
      val year = t.string.matches("(19|20)[0-9][0-9]")
      val isMonthB4 = months.contains(p.string.toLowerCase)
      val isTwoDigits = p.string.split("-").forall(_.matches("[0-9][0-9]?"))
      val isMonth = if (pp.string == "." && pp.hasPrev) months.contains(pp.prev.string.toLowerCase) else months.contains(pp.string.toLowerCase)
      val isMiddleDate = year && isTwoDigits && isMonth || (year && isMonthB4)
      if (isMiddleDate) false
      else {
        val ppp = token.prev.prev.prev
        val pppp = token.prev.prev.prev.prev
        val isTwoDigits1 = p.string.matches("[0-9][0-9]?")
        val isHyphenated = pp.string == "-"
        val isTwoDigits2 = ppp.string.matches("[0-9][0-9]?")
        val isMonth2 = if (pppp.string == "." && pppp.hasPrev) months.contains(pppp.prev.string.toLowerCase) else months.contains(pppp.string.toLowerCase)
        val isMiddleDate2 = year && isTwoDigits1 && isHyphenated && isTwoDigits2 && isMonth2
        !isMiddleDate2
      }
    } else true
  }

  def notTh(token: Token): Boolean = {
    if (token.hasNext && token.next.hasNext) {
      val n = token.next.string.toLowerCase
      val nn = token.next.next.string.toLowerCase
      n != "th" && nn != "th" && n != "nd" && nn != "nd" && n != "th" && nn != "th"
    } else true
  }

  def notEtAl(token: Token): Boolean = {
    if (token.hasPrev && token.hasNext) {
      token.prev.string.toLowerCase != "et" || token.next.string.toLowerCase != "al"
    } else true
  }

  def prevNotMiddleDate(token: Token): Boolean = {
    if (token.hasPrev) notMiddleDate(token.prev) else true
  }

  def notInt(token: Token): Boolean = {
    if (token.hasPrev && token.prev.hasPrev && token.prev.prev.hasPrev) {
      val p = token.prev
      val pp = token.prev.prev
      val ppp = token.prev.prev.prev
      val isL = p.string == "l"
      val isAp = pp.string == "'"
      val isInt = ppp.string.toLowerCase == "int"
      val isInter = isL && isAp && isInt
      !isInter
    } else true
  }

  def notThOrNd(token: Token): Boolean = {
    if (token.hasPrev) {
      val p = token.prev.string.toLowerCase
      !p.matches("[0-9]+(th|nd|st)")
    } else true
  }

  def notTwoWithComma(token: Token): Boolean = {
    if (token.hasNext) {
      val t = token.string.matches("[0-9][0-9]")
      val n = token.next.string == ","
      !(token.string.matches("(0|9)[0-9]") && token.next.string == ",")
    } else true
  }

  def notSame(token: Token): Boolean = {
    if (token.hasNext && token.next.hasNext) {
      val t = token.string.replaceAll("(\\(|\\.|,|\\))", "")
      val n = token.next.string.replaceAll("(\\(|\\.|,|\\))", "")
      val nn = token.next.next.string.replaceAll("(\\(|\\.|,|\\))", "")
      !(t == nn)
    } else true
  }

  def notProc(token: Token): Boolean = {
    if (token.hasPrev && token.prev.hasPrev) {
      val t = token.string.replaceAll("(\\(|\\.|,|\\))", "")
      val p = token.prev.string.replaceAll("(\\(|\\.|,|\\))", "")
      val pp = token.prev.prev.string.replaceAll("(\\(|\\.|,|\\))", "")
      !(t.matches("(19|20)[0-9][0-9]") && (p.toLowerCase == "proc" || pp.toLowerCase == "proc"))
    } else true
  }

  def notBeforeAnd(token: Token, notAuthor: Boolean): Boolean = {
    if (token.hasNext) {
      val n = token.next.string.replaceAll("(\\(|\\.|,|\\))", "")
      !(n == "and" && notAuthor)
    } else true
  }

  def notEdition(token: Token): Boolean = {
    if (token.hasNext) {
      val n = token.next.string.replaceAll("(\\(|\\.|,|\\))", "")
      !(n == "edition")
    } else true
  }

  def notInBefore(token: Token): Boolean = {
    if (token.hasPrev) {
      val p = token.prev.string.replaceAll("(\\(|\\.|,|\\))", "")
      p != "In"
    } else true
  }

  def notZip(token: Token): Boolean = {
    if (token.hasPrev) {
      val p = token.prev.string.replaceAll("(\\(|\\.|,|\\))", "").toLowerCase
      val isZip = token.string.length == 5
      val prevLocations = locations.contains(p)
      !(isZip && prevLocations)
    } else true
  }

  def canFindAl(token: Token): Boolean = {
    var foundAl = false
    var count = 0
    var search = token
    while (search.hasNext && count < 4) {
      if (search.string.toLowerCase.startsWith("al")) foundAl = true
      count += 1
      search = search.next
    }
    foundAl
  }

  def segment(doc: Document): Seq[CitationSpan] = {
    token = doc.tokens.head
    var state: State = new Unstarted(0)
    var notAuthor = false
    var notAuNoAnd = false
    var seenPeriod = false
    val citationSpans = new ArrayBuffer[CitationSpan]()

    while (token != null) {
      if (token.string.contains(".")) seenPeriod = true
      state match {
        case Unstarted(d) =>
          token.string match {
            case startsSeq(s) => state = new Begin(token, s); if (token.string.length == 1) next()
            case _ => state = new Begin(token)
          }
        case Begin(t, "") =>
          if (token.string.startsWith("(") && token.string.length > 1) {
            if (t.position != token.position) prev()
            state = new End(state.asInstanceOf[Begin])
          } else if ((token.string == "." && notThOrNd(token) && notEtAl(token) && notInt(token) && noIndicator(token) && noDate(token)) || (token.string == "," && notBeforeAnd(token, notAuthor) && notMiddleDate(token) && nextNot(token) && noLocation(token)) || (token.string == ":" && notColonExp(token)) || token.string == ";" || token.string == "?" || token.string == ")" || token.string.endsWith(")"))
            state = new End(state.asInstanceOf[Begin])
          /*else if ( (token.string.toLowerCase == "on" || token.string.toLowerCase == "proceedings") && token.position != t.position ) {
            prev
            state = new End(state.asInstanceOf[Begin])
          }*/
          else if (token.string.startsWith("http")) {
            if (t.position == token.position) state = new End(state.asInstanceOf[Begin])
            else {
              prev()
              state = new End(state.asInstanceOf[Begin])
            }
          } else if (token.string.toLowerCase == "preprint") {
            if (t.position == token.position) state = new End(state.asInstanceOf[Begin])
            else {
              prev()
              state = new End(state.asInstanceOf[Begin])
            }
          } else if (token.string.toLowerCase == "report")
            state = new End(state.asInstanceOf[Begin])
          else if (!aggressive && months.contains(token.string.toLowerCase) && token.hasPrev && token.prev.string != "(") {
            if (t.position == token.position) state = new End(state.asInstanceOf[Begin])
            else if (t.position != token.position) {
              prev()
              state = new End(state.asInstanceOf[Begin])
            } else next()
          } else if (!aggressive && token.hasPrev && token.string.matches("[A-Z].+") && token.prev.string.matches("[A-Z]\\.")) {
            if (t.position == token.position) state = new End(state.asInstanceOf[Begin])
            else {
              prev()
              state = new End(state.asInstanceOf[Begin])
            }
          } else if (!aggressive && token.hasNext && token.hasPrev && token.string.matches("[A-Z][a-z]{1,5}") && token.next.string == "." && token.prev.string != "." && token.prev.string != "(" && t.position != token.position) {
            prev()
            state = new End(state.asInstanceOf[Begin])
          } else if (!aggressive && token.string.toLowerCase == "thesis")
            state = new End(state.asInstanceOf[Begin])
          else if (!aggressive && token.hasPrev && token.prev.string.toLowerCase == "submitted" && token.string.toLowerCase == "to")
            state = new End(state.asInstanceOf[Begin])
          else if (!aggressive && (token.string.toLowerCase == "translated" || token.string.toLowerCase == "master" || token.string.toLowerCase == "phd") && t.position != token.position) {
            prev()
            state = new End(state.asInstanceOf[Begin])
          } else if (!aggressive && (token.string.toLowerCase.matches("vol\\.[0-9]+") || token.string.toLowerCase.matches("p[0-9\\-]+")) && t.position != token.position) {
            prev()
            state = new End(state.asInstanceOf[Begin])
          } else if (token.string.toLowerCase.matches("[a-z\\-0-9]+/[0-9]+")) {
            if (t.position == token.position) state = new End(state.asInstanceOf[Begin])
            else {
              prev()
              state = new End(state.asInstanceOf[Begin])
            }
          } else if (token.string.endsWith(","))
            state = new End(state.asInstanceOf[Begin])
          else if (!aggressive && !seenPeriod && token.string.matches("[A-Z][a-z]+"))
            state = new End(state.asInstanceOf[Begin])
          else if (aggressive && (token.string == "In" || ((token.string == "in" || token.string == "of") && token.hasNext && token.next.string == ":")) && canFindEd(token)) {
            token = findEd(token)
            state = new End(state.asInstanceOf[Begin])
          } else if (token.string == "Technical" && token.hasNext && token.next.string == "Report" && t.position != token.position) {
            prev()
            state = new End(state.asInstanceOf[Begin])
          } else if (token.string.toLowerCase == "eds." || token.string.toLowerCase == "ed." || token.string.matches("[A-Z][A-Z]\\.")) {
            state = new End(state.asInstanceOf[Begin])
          } else if (token.string == "of" && token.hasNext && (token.hasPrev && !token.prev.string.toLowerCase.startsWith("proc") && token.prev.string.toLowerCase != "in" && (token.next.string == "Lecture" || token.next.string == "Lec" || (token.next.string.matches("[A-Z]*") && token.next.string.length > 3))) && t.position != token.position) {
            prev()
            state = new End(state.asInstanceOf[Begin])
          } else if (token.string == "in" && token.hasNext && (token.next.string == "LNCS" || token.next.string == "LNAI" || token.next.string == "Lecture" || (token.next.string.matches("[A-Z]*") && token.next.string.length > 3)) && t.position != token.position) {
            prev()
            state = new End(state.asInstanceOf[Begin])
          } else if ((token.string.toLowerCase == "in" || token.string.toLowerCase == "of") && token.position == t.position) {
            if (token.hasNext && token.next.string == "book") next()
            state = new End(state.asInstanceOf[Begin])
          } else if (token.string == "as" && token.hasPrev && (token.prev.string == "Published") && t.position != token.position) {
            state = new End(state.asInstanceOf[Begin])
          } else if (months.contains(token.string.toLowerCase) && t.string != "(" && token.hasPrev && token.prev.string != "(" && token.prev.string != "the" && token.hasNext && token.next.string.toLowerCase != "school" && t.position != token.position) {
            prev()
            state = new End(state.asInstanceOf[Begin])
          } else if (token.string == "," && !noLocation(token) && findLocation(token) != null && findEndLocation(token) != null) {
            var newt = findLocation(token)
            if (t == newt || token.position - t.position <= 4) {newt = findEndLocation(token)} else newt = newt.prev
            if (newt != null) {
              token = newt
              state = new End(state.asInstanceOf[Begin])
            }
          } else if ((token.string == "#" || token.string.startsWith("(")) && t.position != token.position) {
            prev()
            state = new End(state.asInstanceOf[Begin])
          } else if ((token.string == "Vol." || token.string == "vol" || ((token.string == "No." || token.string == "No") && token.hasPrev && token.prev.string != "Report") || ((token.string == "The" || token.string == "A") && token.hasPrev && token.prev.string.trim != "," && token.prev.string.toLowerCase != "in") || token.string == "pp" || token.string == "pp.") && (t.position != token.position && token.hasPrev && token.prev.string != "(")) {
            prev()
            state = new End(state.asInstanceOf[Begin])
          } else if (isAbr(token) && notHyphen(token) && token.hasNext && token.next.string != "in" && token.next.string != "of" && !notAuNoAnd) {
            if (!aggressive) {
              if (t.position != token.position) prev()
            }
            while (aggressive && token.hasNext && (isAbr(token.next) || token.next.string == "-")) token = token.next
            state = new End(state.asInstanceOf[Begin])
          } else if (token.string.matches("[A-Z][a-z]+") && token.hasPrev && token.prev.string.matches("[a-z][a-z]+") && token.prev.string != "of" && token.prev.string != "on" && token.prev.string != "the" && token.prev.hasPrev && token.prev.prev.string.matches("[a-z][a-z]+") && !token.prev.string.endsWith("th") && token.prev.string != "an" && token.prev.prev.string != "of" && token.prev.string != "and" && token.position != t.position) {
            prev()
            state = new End(state.asInstanceOf[Begin])
          } else if (token.string == "et" && canFindAl(token) && t.position != token.position) {
            prev()
            state = new End(state.asInstanceOf[Begin])
          } else if (((token.string.toLowerCase == "and" || token.string.toLowerCase == "und" || token.string.toLowerCase == "&") && notAndExclude(token) && !notAuthor) || (isDigits(token) &&
            notProc(token) && notInBefore(token) && notSame(token) && notEdition(token) && notZip(token) && notTwoWithComma(token) && notTh(token) && prevNotMiddleDate(token) && hasHyphen(token) && prevNotDate(token) && (token.hasPrev && token.prev.string != "the" && token.prev.string != "of" && token.prev.string != "(" && noDate(token.prev))) || (token.string.matches(startsSeq.toString()) && notEditor(token))) {
            if (t.position == token.position || !prevNotPagesOrAp(token)) {
              if (token.hasNext && (token.next.string == "a" || token.next.string == "b")) next()
              state = new End(state.asInstanceOf[Begin])
            } else {
              prev()
              state = new End(state.asInstanceOf[Begin])
            }
          } else if (!token.hasNext)
            state = new End(state.asInstanceOf[Begin])
          else next()
        case Begin(t, o) =>
          val e = if (token.string.replaceAll("(,|\\.)", "").length > 0) token.string.replaceAll("(,|\\.)", "").last.toString else ""
          val ends = ender(o)
          if (ends.contains(e) || ends.contains(token.string)) {
            state = new End(state.asInstanceOf[Begin])
          } else {
            token = if (token.hasNext) token.next else null
          }
        case End(e) =>
          while (aggressive && token.hasNext && (token.next.string == "." || token.next.string == "(ed.)" || token.next.string == "eds." || token.next.string == "(eds.)" || token.next.string == "(editors)")) next
          while (token.hasNext && (token.next.string == "." || token.next.string.trim == "" || token.next.string.toLowerCase == "inc" || token.next.string == ")" || token.next.string == ";" || token.next.string == "," || token.next.string == ":")) next
          val cs = new CitationSpan(doc, "I-booktitle", e.start.position, token.position - e.start.position + 1)
          citationSpans += cs
          if (notAuthorCheck(cs)) notAuthor = aggressive
          if (notAuthorNoAndCheck(cs)) notAuNoAnd = aggressive
          for (t <- cs.tokens; if t.string.contains(".")) seenPeriod = true
          if (token.hasNext) {
            token = token.next
            token.string match {
              case startsSeq(s) => state = new Begin(token, s); if (token.string.length == 1) next()
              case _ => state = new Begin(token)
            }
          } else token = null
      }
    }
    citationSpans
  }
}

case class CitationSpanList(spans: Seq[CitationSpan])

object OverSegmenter {

  private val logger = Logger.getLogger(getClass.getName)

  def overSegment(documents: Seq[Document], lexiconUrlBase: String, pool: Option[ExecutorService] = None) {

    val sources = Array(
      "known_country.lst",
      "known_place.lst",
      "known_state.lst",
      "WikiLocations.lst",
      "WikiLocationsRedirects.lst"
    ).map(lexiconUrlBase + "/" +)

    val segmenter = new OverSegmenter(sources, Array(lexiconUrlBase + "/months.lst"))
    logger.info("Segmenting citations")

    def processDoc(x: Document) = {
      try {
        val spans = segmenter.segment(x)

        // TODO what does this even do?
        copyToSeg(x, spans)
        x.attr += CitationSpanList(spans)

        //x.asSection.spansOfClass[CitationSpan].foreach( _.delete(null) )
      } catch {
        case npe: NullPointerException => println(npe.getMessage)
        case iob: ArrayIndexOutOfBoundsException => println(iob.getMessage)
      }
    }

    if (pool.isDefined) {
      cc.factorie.util.Threading.parForeach(documents, pool.get)(processDoc(_))
    } else {
      documents.foreach(processDoc)
    }
    println("Segmentation complete")
  }

  //TODO what does this do?
  def copyToSeg(doc: Document, spans: Seq[CitationSpan]) {
    for (span <- spans) {
      new SegmentSpan(span.document, span.start, span.length)(null)
    }
  }

  def copyToSeg2(doc: Document, spans: Seq[CitationSpan]): Seq[SegmentSpan] = {
    spans.map { span =>
      new SegmentSpan(span.document, span.start, span.length)(null)
    }
  }
}

class OverSegmenterAnnotator(lexiconUrlBase: String) extends DocumentAnnotator {

  val sources = Array(
    "known_country.lst",
    "known_place.lst",
    "known_state.lst",
    "WikiLocations.lst",
    "WikiLocationsRedirects.lst"
  ).map(lexiconUrlBase + "/" +)

  val segmenter = new OverSegmenter(sources, Array(lexiconUrlBase + "/months.lst"))

  def process(document: Document): Document = {
    try {
      val spans = segmenter.segment(document)
      copyToSeg(document, spans)
      document.attr += CitationSpanList(spans)
    } catch {
      case npe: NullPointerException => println(npe.getMessage)
      case iob: ArrayIndexOutOfBoundsException => println(iob.getMessage)
    }
    document
  }

  //TODO what does this do?
  def copyToSeg(doc: Document, spans: Seq[CitationSpan]) {
    for (span <- spans) {
      new SegmentSpan(span.document, span.start, span.length)(null)
    }
  }

  def prereqAttrs: Iterable[Class[_]] = ???

  def postAttrs: Iterable[Class[_]] = Seq(classOf[CitationSpanList])

  def tokenAnnotationString(token: Token): String = ???
}
package scitzen.parser

import de.rmgk.scip.*
import scitzen.sast.{Attribute, Attributes, Inline, Text}
import scitzen.parser.CommonParsers.*

import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

object AttributesParser {

  inline val open  = "{"
  inline val close = "}"

  val terminationCheckB: Scip[Boolean] = (";".all or close.all or eol).lookahead
  val unquotedInlines: Scip[List[Inline]] =
    InlineParsers.full(terminationCheckB, allowEmpty = true)

  /** text is in the general form of ""[content]"" where all of the quoting is optional,
    * but the closing quote must match the opening quote
    */
  val text: Scip[Text] = Scip {
    anySpacesF.run
    val r = ("\"".all.rep.dropstr.run, "[".all.str.opt.run) match {
      case ("", None) => unquotedInlines
      case (quotes, bracket) =>
        val closing = bracket.fold(quotes)(_ => s"]$quotes")
        InlineParsers.full(
          (seq(closing).trace("closing") and verticalSpaces.trace("spaces") and terminationCheckB.trace(
            "terminate"
          )).trace("endingfun"),
          allowEmpty = true
        ).trace("inline full")
    }
    scitzen.sast.Text(r.run)
  }.trace("text")

  val stringValue: Scip[String] = Scip {
    anySpacesF.run
    val quotes  = "\"".all.rep.min(0).str.trace(s"opening quotes").run
    val bracket = "[".all.str.opt.trace("opening brackets").run
    if quotes.isEmpty && bracket.isEmpty
    then until(";}\n".any).min(0).str.trace(s"unquoted").run
    else
      val b = bracket.fold("")(_ => "]")
      (until(seq(s"$b$quotes") and verticalSpaces and terminationCheckB).min(1).str <~ seq(s"$b$quotes").orFail).trace(
        s"quoted ${s"$b$quotes"}"
      ).run
  }.trace("string value")

  val namedAttributeValue: Scip[Either[Seq[Attribute], String]] =
    (anySpacesB ifso braces.map(Left.apply)) | stringValue.map(Right.apply)

  val namedAttributeStart: Scip[String] = Scip {
    verticalSpaces.orFail.run
    val id = identifierB.str.run
    verticalSpaces.orFail.run
    "=".all.orFail.run
    id
  }

  val namedAttribute: Scip[Attribute] = Scip {
    val id = namedAttributeStart.run
    namedAttributeValue.trace("attr value").run match {
      case Left(attr)   => scitzen.sast.Attribute.Nested(id, Attributes(attr))
      case Right(value) => scitzen.sast.Attribute.Plain(id, value)
    }
  }.trace("named attr")

  val positionalAttribute: Scip[Attribute] = Scip {
    val begin    = scx.index
    val mtxt     = text.run
    val contents = scx.str(begin, scx.index)
    scitzen.sast.Attribute.Positional(mtxt, Some(contents))
  }.trace("pos attr")

  val attribute: Scip[Attribute] = (namedAttribute | positionalAttribute).trace("attribute")

  def listOf(elem: Scip[Attribute], min: Int): Scip[Seq[Attribute]] =
    (elem.list(";\n".any).require(_.sizeIs >= min) <~ ";".all.trace("list end attempt")).trace("list of")

  val braces: Scip[Seq[Attribute]] = Scip {
    open.all.orFail.run
    anySpacesF.run
    val res = listOf(attribute, min = 0).trace("bracelist").run
    anySpacesF.run
    close.all.orFail.run
    res
  }.trace("braces")

  val noBraces: Scip[Seq[Attribute]] = Scip {
    val res = listOf(namedAttribute, min = 1).run
    spaceLineF.run
    spaceLineF.run
    res
  }.trace("no braces")

  val configFile: Scip[Seq[Attribute]] = Scip {
    val res = noBraces.opt.map(_.getOrElse(Nil)).run
    anySpacesF.run
    end.orFail.run
    res
  }

}

object AttributeDeparser {
  // these close unquoted attributes, so need quoting
  inline def needQuotesInner: Scip[Boolean] = "\n;}".any

  // count the number of closing delimiter looking "
  inline def closingRun: Scip[Int] = Scip {
    if "]".all.run
    then "\"".all.rep.run
    else -1
  }

  case class QuoteInfo(confusable: Boolean, length: Int)

  def getInfo: Scip[QuoteInfo] = Scip {

    val looksLikeAttributeStart = AttributesParser.namedAttributeStart.attempt.run

    var inner   = false
    var longest = 0
    @tailrec def loop(): Unit =
      if end.run
      then ()
      else
        // try to parse directives first, as they do not need to be quoted
        DirectiveParsers.comment.attempt.run
        DirectiveParsers.full.attempt.run
        if needQuotesInner.run
        then
          inner = true
          loop()
        else
          val cr = closingRun.run
          if cr > longest
          then longest = cr

          if cr == -1
          then scx.next

          loop()
    loop()

    QuoteInfo(inner || looksLikeAttributeStart, longest)

  }

  def quote(value: String): String =
    val info = AttributeDeparser.getInfo.runInContext(Scx(value).copy(tracing = false))
    value.headOption match
      case None => "" // empty value
      case Some(ch) =>
        val someQuotes = ch == ' ' || info.confusable || ch == '{'
        if
          ch == '[' ||
          ch == '"' ||
          (someQuotes && info.length >= 1)
        then
          val quotes = "\"".repeat(math.max(0, info.length))
          s"""$quotes[$value]$quotes"""
        else if someQuotes
        then s""""$value""""
        else value

}

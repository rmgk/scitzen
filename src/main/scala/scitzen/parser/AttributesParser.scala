package scitzen.parser

import de.rmgk.scip.*
import scitzen.parser.CommonParsers.*
import scitzen.sast.{Attribute, Attributes, Inline, Text}

import java.nio.charset.StandardCharsets
import scala.util.matching.Regex
import scala.util.{Success, Try}

object AttributesParser {

  inline val open  = "{"
  inline val close = "}"

  val terminationCheckB: Scip[Boolean] = (";".all or close.all or eol).lookahead
  val unquotedInlines: Scip[List[Inline]] =
    InlineParsers.full(terminationCheckB)

  val quotes: Scip[String] = Scip {
    anySpacesF.run
    val quotes = (("\"".all.rep.min(1) and "[".all) or "\"".all).str.trace(s"opening quotes").run
    if quotes.endsWith("[")
    then s"]${quotes.stripSuffix("[")}"
    else quotes

  }

  /** text is in the general form of ""[content]"" where all of the quoting is optional,
    * but the closing quote must match the opening quote
    */
  val text: Scip[Text] = Scip {
    val r = quotes.opt.run match
      case None =>
        verticalSpaces.run
        unquotedInlines.run
      case Some(closing) =>
        val closeP = seq(closing)
        InlineParsers.full(
          (
            closeP.trace("closing") and
            verticalSpaces.trace("spaces") and
            terminationCheckB.trace("terminate")
          ).trace("endingfun")
        ).trace("inline full").run
    scitzen.sast.Text(r)
  }.trace("text")

  val namedAttributeValue: Scip[Either[Seq[Attribute], Text]] =
    (anySpacesB ifso (braces.map(Left.apply) | text.map(Right.apply)))

  val namedAttributeStart: Scip[String] = Scip {
    verticalSpaces.orFail.run
    val id = identifierB.str.run
    verticalSpaces.orFail.run
    "=".all.orFail.run
    id
  }

  val namedAttribute: Scip[Attribute] = Scip {
    val id = namedAttributeStart.run
    val start = scx.index
    val res = namedAttributeValue.trace("attr value").run
    val raw = scx.str(start, scx.index)
    res match {
      case Left(attr)   =>
        assert(attr.sizeIs <= 1, s"nested attributes no longer supported: $attr")
        if attr.isEmpty
        then
          Attribute(id, "", Text.empty)
        else
          Attribute(id, attr.head.raw, attr.head.text)
      case Right(text) => Attribute(id, raw, text)
    }
  }.trace("named attr")

  val positionalAttribute: Scip[Attribute] = Scip {
    val start = scx.index
    val mtxt = text.run
    val raw = scx.str(start, scx.index)
    Attribute("", raw, mtxt)
  }.trace("pos attr")

  val attribute: Scip[Attribute] = (namedAttribute | positionalAttribute).trace("attribute")

  def listOf(elem: Scip[Attribute], min: Int): Scip[Seq[Attribute]] =
    (elem.list(";\n".any).require(_.sizeIs >= min) <~ ";".all.trace("list end attempt")).trace("list of")

  val braces: Scip[Seq[Attribute]] = Scip {
    open.all.orFail.run
    val res = listOf(attribute, min = 0).trace("bracelist").run
    anySpacesF.run
    close.all.orFail.run
    res
  }.trace("braces")

  val noBraces: Scip[Seq[Attribute]] = Scip {
    val res = listOf(namedAttribute, min = 1).run
    spaceLineF.run
    spaceLineB.run
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

  val countQuotes: Regex = """(]"+)""".r

  def quote(forceEmpty: Boolean, value: String, check: Text => Boolean): String =

    // empty value is always represented using quotes for clarity
    if forceEmpty && value.isEmpty then return "\"\""

    def parses(str: String): Boolean =
      Try {
        (AttributesParser.attribute <~ de.rmgk.scip.end.orFail).opt.runInContext(Scx(str).copy(tracing = false))
      } match
        case Success(Some(Attribute("", _, text))) if check(text) => true
        case other                                          => false

    def pickFirst(candidate: List[() => String]): Option[String] =
      candidate.view.map(_.apply()).find(parses)

    val candidates =
      if value.startsWith(" ") || value.startsWith("{")
      then List(() => s""""$value"""")
      else List(() => value, () => s""""$value"""")

    pickFirst(candidates).getOrElse {
      val mi = countQuotes.findAllIn(value)
      val quoteCount = if mi.isEmpty then 1
      else mi.map(_.length).max
      val quotes = "\"" * quoteCount
      s"""$quotes[$value]$quotes"""
    }

}

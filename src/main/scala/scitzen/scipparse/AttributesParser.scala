package scitzen.scipparse

import de.rmgk.scip.*
import scitzen.sast.{Attribute, Attributes, Inline, Text}
import scitzen.scipparse.CommonParsers.*

import java.nio.charset.StandardCharsets

object AttributesParser {

  inline val open  = "{"
  inline val close = "}"

  val terminationCheckB: Scip[Boolean] = (";".all or close.all or eolB).lookahead
  val terminationCheck: Scip[Unit] = terminationCheckB.orFail
  val unquotedInlines               = InlineParsers.full((";\n"+close).any, terminationCheckB, allowEmpty = true)
  val unquotedText   : Scip[Seq[Inline]] = unquotedInlines.trace("unquoted")

  /** text is in the general form of ""[content]"" where all of the quoting is optional,
    * but the closing quote must match the opening quote
    */
  val text: Scip[Text] = Scip {
    anySpaces.run
    val r = ("\"".all.rep.dropstr.run, "[".all.str.opt.run) match {
      case ("", None) => unquotedText
      case (quotes, bracket) =>
        val closing = bracket.fold(quotes)(_ => s"]$quotes")
        val closingByte = closing.substring(0, 1).getBytes(StandardCharsets.UTF_8).head
        InlineParsers.full(
          bpred(_ == closingByte),
          (seq(closing).trace("closing") and verticalSpacesB.trace("spaces") and terminationCheckB.trace("terminate")).trace("endingfun"),
          allowEmpty = true
        ).trace("inline full")
    }
    scitzen.sast.Text(r.run)
  }.trace("text")

  val stringValue: Scip[String] = Scip {
    anySpaces.run
    val quotes  = "\"".all.rep.min(0).str.trace(s"kv q").run
    val bracket = "[".all.str.opt.trace("kv b").run
    if quotes.isEmpty && bracket.isEmpty
    then until(";}\n".any).min(0).str.trace(s"unquoted").run
    else
      val b = bracket.fold("")(_ => "]")
      (until(seq(s"$b$quotes") and verticalSpacesB and terminationCheckB).min(1).str <~ seq(s"$b$quotes").orFail).trace(s"quoted ${s"$b$quotes"}").run
  }.trace("string value")

  val namedAttributeValue: Scip[Either[Seq[Attribute], String]] =
    (anySpacesB ifso braces.map(Left.apply)) | stringValue.map(Right.apply)

  val namedAttribute: Scip[Attribute] = Scip {
    verticalSpacesB.orFail.run
    val id = identifierB.str.run
    verticalSpacesB.orFail.run
    "=".all.orFail.run
    namedAttributeValue.trace("attr value").run match {
      case Left(attr)   => scitzen.sast.Attribute.Nested(id, Attributes(attr))
      case Right(value) => scitzen.sast.Attribute.Plain(id, value)
    }
  }.trace("named attr")

  val positionalAttribute: Scip[Attribute] = Scip {
    val begin = scx.index
    val mtxt = text.run
    val contents = scx.str(begin, scx.index)
    scitzen.sast.Attribute.Positional(mtxt, Some(contents))
  }.trace("pos attr")

  val attribute: Scip[Attribute] = (namedAttribute | positionalAttribute).trace("attribute")

  def listOf(elem: Scip[Attribute], min: Int): Scip[Seq[Attribute]] =
    (elem.list(";\n".any).require(_.sizeIs >= min) <~ ";".all.trace("list end attempt")).trace("list of")

  val braces: Scip[Seq[Attribute]] = Scip {
    open.all.orFail.run
    anySpaces.run
    val res = listOf(attribute, min = 0).trace("bracelist").run
    anySpaces.run
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
    anySpaces.run
    res
  }

}

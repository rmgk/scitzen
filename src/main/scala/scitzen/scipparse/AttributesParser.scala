package scitzen.scipparse

import scitzen.sast.{Attribute, Attributes, Inline, Text}
import de.rmgk.scip.*
import CommonParsers.*
import CompatParsers.*

import java.nio.charset.StandardCharsets

object AttributesParser {

  inline val open  = "{"
  inline val close = "}"

  val terminationCheckB: Scip[Boolean] = (";".all or close.all or eolB).lookahead
  val terminationCheck: Scip[Unit] = terminationCheckB.orFail
  val unquotedInlines: InlineParsers               = InlineParsers((";\n"+close).any, terminationCheckB, allowEmpty = true)
  val unquotedText   : Scip[(Seq[Inline], String)] = unquotedInlines.full.trace("unquoted")

  /** text is in the general form of ""[content]"" where all of the quoting is optional,
    * but the closing quote must match the opening quote
    */
  val text: Scip[Text] = Scip {
    anySpaces.run
    val r = ("\"".all.rep.dropstr.run, "[".all.str.opt.run) match {
      case ("", None) => unquotedText
      case (quotes, bracket) =>
        val closing = bracket.fold(quotes)(_ => s"]$quotes")
        val myend = closing.substring(0, 1).getBytes(StandardCharsets.UTF_8).head
        InlineParsers(
          bpred(_ == myend),
          exact(closing) and verticalSpacesB and terminationCheckB,
          allowEmpty = true
        ).full.trace("inline full")
    }
    scitzen.sast.Text(r.run._1)
  }.trace("text")

  val stringValue: Scip[String] = Scip {
    anySpaces.run
    val quotes  = "\"".all.rep.min(0).str.trace(s"kv q").run
    val bracket = "[".all.str.opt.trace("kv b").run
    if quotes.isEmpty && bracket.isEmpty
    then until(";}\n".any).min(0).str.trace(s"unquoted").run
    else
      val b = bracket.fold("")(_ => "]")
      (until(exact(s"$b$quotes") and verticalSpacesB and terminationCheckB).min(1).str <~ exact(s"$b$quotes")).run
  }.trace("string value")

  val namedAttributeValue: Scip[Either[Seq[Attribute], String]] =
    choice(anySpaces ~> braces.map(Left.apply), stringValue.map(Right.apply))

  val namedAttribute: Scip[Attribute] = Scip {
    verticalSpaces.trace("vertical spaces?").run
    val id = identifierB.str.trace("attr ident").run
    verticalSpaces.run
    "=".all.orFail.run
    // there was a cut here once … ?
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

  val attribute: Scip[Attribute] = choice(namedAttribute, positionalAttribute).trace("attribute")

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
    spaceLine.run
    spaceLine.run
    res
  }.trace("no braces")

  val configFile: Scip[Seq[Attribute]] = Scip {
    val res = noBraces.opt.map(_.getOrElse(Nil)).run
    anySpaces.run
    res
  }

}

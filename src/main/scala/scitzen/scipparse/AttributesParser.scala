package scitzen.scipparse

import scitzen.sast.{Attribute, Attributes, Inline, Text}
import de.rmgk.scip.*
import CommonParsers.*
import CompatParsers.*

import java.nio.charset.StandardCharsets

object AttributesParser {

  inline val open  = "{"
  inline val close = "}"

  val start: Scip[Unit] = open.scip

  def terminationCheck                          = choice(";".scip, close.scip, eol).lookahead
  val unquotedInlines                           = InlineParsers(s";\n$close", terminationCheck, allowEmpty = true)
  val unquotedText: Scip[(Seq[Inline], String)] = unquotedInlines.full.named("unquoted")

  /** text is in the general form of ""[content]"" where all of the quoting is optional,
    * but the closing quote must match the opening quote
    */
  val text: Scip[Text] = Scip {
    anySpaces.run
    val r = ("\"".scip.rep(0).!.run, "[".scip.!.opt.run) match {
      case ("", None) => unquotedText
      case (quotes, bracket) =>
        val closing = bracket.fold(quotes)(_ => s"]$quotes")
        InlineParsers(closing.substring(0, 1), closing.scip ~ verticalSpaces ~ terminationCheck, allowEmpty = true).full.named("inline full")
    }
    scitzen.sast.Text(r.run._1)
  }.named("text")

  val stringValue: Scip[String] = Scip {
    anySpaces.run
    ("\"".scip.rep(1).!.run, "[".scip.!.opt.run) match {
      case ("", None) => CharsWhile(c => c != ';' && c != '}' && c != '\n', 1).?.map(_ => ()).!.run
      case (quotes, bracket) =>
        val b = bracket.fold("")(_ => "]")
        val r = untilE(exact(s"$b$quotes".getBytes(StandardCharsets.UTF_8)) ~ verticalSpaces ~ terminationCheck).run
        s"$b$quotes".scip.run
        r
    }
  }

  val namedAttributeValue: Scip[Either[Seq[Attribute], String]] =
    choice(Scip { anySpaces.run; braces.map(Left.apply).run }, stringValue.map(Right.apply))

  val namedAttribute: Scip[Attribute] = Scip {
    verticalSpaces.run
    val id = identifier.!.run
    verticalSpaces.run
    "=".scip.run
    // there was a cut here once … ?
    namedAttributeValue.run match {
      case Left(attr)   => scitzen.sast.Attribute.Nested(id, Attributes(attr))
      case Right(value) => scitzen.sast.Attribute.Plain(id, value)
    }
  }.named("named attr")

  val positionalAttribute: Scip[Attribute] = Scip {
    // unclear to me if there is any way to acquire a parsed value AND the parsed string from fastparse
    var hack: Text = null
    text.map { v => hack = v }.!.map { value =>
      scitzen.sast.Attribute.Positional(hack, Some(value))
    }.run
  }.named("pos attr")

  val attribute: Scip[Attribute] = choice(namedAttribute, positionalAttribute)

  def listOf(elem: Scip[Attribute], min: Int): Scip[Seq[Attribute]] = Scip {
    val res = repeat(elem.named("list elem"), choice(";".scip, newline), min = min).run
    ";".scip.?.run
    res
  }.named("list of")

  def braces: Scip[Seq[Attribute]] = Scip {
    open.scip.run
    anySpaces.run
    val res = listOf(attribute, min = 0).run
    anySpaces.run
    close.scip.run
    res
  }.named("braced")

  val noBraces: Scip[Seq[Attribute]] = Scip {
    val res = listOf(namedAttribute, min = 1).run
    (spaceLine ~ spaceLine).run
    res
  }

  val configFile: Scip[Seq[Attribute]] = Scip {
    val res = noBraces.opt.map(_.getOrElse(Nil)).run
    anySpaces.run
    res
  }

}

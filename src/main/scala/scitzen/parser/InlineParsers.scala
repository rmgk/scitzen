package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.parser.MacroCommand.{Comment, Math}


object InlineParsers {

  def quoteChars[_: P]: P[Unit] = CharIn("_*`$")
  def commentStart[_: P]: P[Unit] = P(":%")

  def specialChars[_: P]: P[Unit] = CharIn("_*`$%")


  def syntaxStart[_: P]: P[Unit] = P(":" ~ (specialChars | MacroParsers.detectStart))
  def texStart[_: P]: P[Unit] = P("\\" ~ (identifier ~ "{"))

  // grab everything until a unconstrained position followed by a syntax starter
  // include the unconstrained position
  // the until fails if empty, in that was we are just now at a potential syntax start,
  // so eat that and return
  private def notSyntax[_: P]: P[String] = P(untilE(End | syntaxStart | texStart | "$"))

  def simpleText[_: P]: P[InlineText] = {
    P(notSyntax.!).map(InlineText)
  }

  def comment[_: P]: P[Macro] = P(Index ~ commentStart ~ untilI(eol, 0).! ~ Index)
  .map { case (s, text, e) => Macro(Comment, Attribute("", text).toAttributes(Prov(s, e))) }

  def fullParagraph[_: P]: P[Seq[Inline]] =
    P(inlineSequence.? ~ End)
    .map(_.getOrElse(Nil))

  def texDollar[_: P]: P[Macro] = P(withProv("$" ~ untilI("$", 0))).map {
    case (q, p) => Macro(Math, Attribute("", q).toAttributes(p))
  }

  def inlineSequence[_: P]: P[Seq[Inline]] = P {
    (comment
     | MacroParsers.full
     | MacroParsers.texHack
     | texDollar
     | quoted
     | simpleText
    ).rep(1)
  }

  def quoted[_: P]: P[Macro] = P {
    withProv {
      CharIn(":") ~ quoteChars.!.flatMap { delimiter =>
        (!(delimiter | anySpace) ~ untilE(delimiter))
        .! ~ delimiter.!
      }
    }.map { case ((v, delimiter), prov) =>
      Macro(MacroCommand.parse(delimiter), Attribute("", v).toAttributes(prov)) }
  }
}

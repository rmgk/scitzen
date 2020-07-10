package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.parser.MacroCommand.{Comment}


object InlineParsers {

  def commentStart[_: P]: P[Unit] = P(":%")

  def syntaxStart[_: P]: P[Unit] = P(commentStart | MacroParsers.detectStart)

  private def notSyntax[_: P]: P[String] = P(untilE(End | syntaxStart))

  def simpleText[_: P]: P[InlineText] = {
    P(notSyntax.!).map(InlineText)
  }

  def comment[_: P]: P[Macro] = P(withProv(commentStart ~ untilI(eol).!))
  .map { case (text, prov) => Macro(Comment, Attribute("", text).toAttributes(prov)) }

  def fullParagraph[_: P]: P[Seq[Inline]] =
    P(inlineSequence ~ End)

  def inlineSequence[_: P]: P[Seq[Inline]] = P {
    (comment
     | MacroParsers.full
     | simpleText
    ).rep(0)
  }
}

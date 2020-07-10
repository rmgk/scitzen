package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object InlineParsers {

  private def notSyntax[_: P]: P[String] = P(untilE(End | MacroParsers.syntaxStart))

  def simpleText[_: P]: P[InlineText] = {
    P(notSyntax.!).map(InlineText)
  }

  def fullParagraph[_: P]: P[Seq[Inline]] =
    P(inlineSequence ~ End)

  def inlineSequence[_: P]: P[Seq[Inline]] =
    P((MacroParsers.comment | MacroParsers.full | simpleText).rep(0))
}

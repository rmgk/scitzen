package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._

object InlineParsers {

  private def notSyntax[_: P]: P[String] =
    P((CharsWhile(_ != ':') ~ (!MacroParsers.syntaxStart ~ ":").rep(0)).rep(
      min = 1,
      sep = !MacroParsers.syntaxStart
    ).!)

  def simpleText[_: P]: P[InlineText] = {
    P(notSyntax.!).map(InlineText)
  }

  def fullParagraph[_: P]: P[Seq[Inline]] =
    P(inlineSequence ~ End)

  def inlineSequence[_: P]: P[Seq[Inline]] =
    P((MacroParsers.comment | MacroParsers.full | simpleText).rep(0))
}

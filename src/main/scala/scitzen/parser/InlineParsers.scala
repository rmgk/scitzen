package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers.{eol, untilE, withProv}
import scitzen.parser.MacroCommand.Comment
import scitzen.parser.MacroParsers.commentStart
import scitzen.parser.Sast.Macro

object InlineParsers {

  private def notSyntax[_: P]: P[String] =
    P((CharsWhile(_ != ':') | (!MacroParsers.syntaxStart ~ ":")).rep(1).!)

  def simpleText[_: P]: P[InlineText] = {
    P(notSyntax.!).map(InlineText)
  }

  def inlineSequence[_: P]: P[Seq[Inline]] =
    P((MacroParsers.comment | MacroParsers.full | simpleText).rep(0))

  def fullParagraph[_: P]: P[Seq[Inline]] =
    P(inlineSequence ~ End)
}

case class EndedInlineParsers(endChars: String, endingFun: P[_] => P[Unit]) {

  def ending[_: P]: P[Unit] = endingFun(implicitly)

  def comment[_: P]: P[Macro] =
    P(withProv(commentStart ~ (untilE(eol, min = 0) ~ (&(ending) | eol)).!))
      .map { case (text, prov) => Macro(Comment, Attribute("", text).toAttributes(prov)) }

  private def notSyntax[_: P]: P[String] =
    P((
      CharsWhile(c => c != ':' && !endChars.contains(c)) | (!MacroParsers.syntaxStart ~ ":") | (!ending ~ endChars)
    ).rep(1).!)

  def simpleText[_: P]: P[InlineText] = {
    P(notSyntax.!).map(InlineText)
  }

  def inlineSequence[_: P]: P[Seq[Inline]] =
    P((comment | MacroParsers.full | simpleText).rep(1))

  def full[_: P]: P[(Seq[Inline], String)] =
    P(inlineSequence ~ ending.!)

}

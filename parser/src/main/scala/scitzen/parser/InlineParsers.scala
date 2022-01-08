package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers.{eol, untilE, withProv}
import scitzen.sast.MacroCommand.Comment
import scitzen.parser.MacroParsers.commentStart
import scitzen.sast.{Attribute, Inline, InlineText, Macro}

case class InlineParsers(endChars: String, endingFun: P[_] => P[Unit], allowEmpty: Boolean = false) {

  def ending[_p: P]: P[Unit] = endingFun(implicitly)

  def comment[_p: P]: P[Macro] =
    P(withProv(commentStart ~ (untilE(eol, min = 0) ~ (&(ending) | eol)).!))
      .map { case (text, prov) => Macro(Comment, Attribute("", text).toAttributes)(prov) }

  private def notSyntax[_p: P]: P[String] =
    P((
      CharsWhile(c => c != ':' && !endChars.contains(c)) | (!MacroParsers.syntaxStart ~ ":") | (!ending ~ CharPred(
        endChars.contains(_)
      ))
    ).rep(1).!)

  def simpleText[_p: P]: P[InlineText] = {
    P(notSyntax.!).map(InlineText)
  }

  def inlineSequence[_p: P]: P[Seq[Inline]] =
    P((comment | MacroParsers.full | simpleText).rep(if (allowEmpty) 0 else 1))

  def full[_p: P]: P[(Seq[Inline], String)] =
    P(inlineSequence ~ ending.!)

}

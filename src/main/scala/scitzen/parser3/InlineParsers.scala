package scitzen.parser3

import cats.parse.{Numbers, Rfc5234, Parser as P, Parser0 as P0}
import cats.syntax.*
import cats.implicits.*
import cats.parse.Numbers.digits
import cats.parse.Rfc5234.sp
import cats.parse.Parser.*
import CommonParsers.{eol, withProv, stringParser}
import scitzen.sast.MacroCommand.Comment
import MacroParsers.commentStart
import scitzen.sast.{Attribute, Inline, InlineText, Macro}

case class InlineParsers(endChars: String, ending: P0[Unit], allowEmpty: Boolean = false) {

  val comment: P[Macro] =
    (withProv(commentStart *> (until0(eol) ~ (peek(ending) | eol)).string))
      .map { (text, prov) => Macro(Comment, Attribute("", text).toAttributes, prov) }

  private val notSyntax: P[String] =
    (
      charsWhile(c => c != ':' && !endChars.contains(c)).void |
        (not(scitzen.parser3.MacroParsers.syntaxStart).with1 ~ ":").void |
        (not(ending).with1 *> charWhere(endChars.contains(_))).void
    ).rep(1).string

  val simpleText: P[InlineText] = {
    (notSyntax).map(InlineText)
  }

  val inlineSequence: P[List[Inline]] =
    ((comment | scitzen.parser3.MacroParsers.full | simpleText).rep(if (allowEmpty) 0 else 1)).map(_.toList)

  val full: P[(Seq[Inline], String)] =
    (inlineSequence ~ ending.string)

}

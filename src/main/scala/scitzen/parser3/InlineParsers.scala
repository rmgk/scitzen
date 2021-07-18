package scitzen.parser3

import cats.parse.{Numbers, Rfc5234, Parser as P, Parser0 as P0}
import cats.syntax.*
import cats.implicits.*
import cats.parse.Numbers.digits
import cats.parse.Rfc5234.sp
import cats.parse.Parser.*
import scitzen.parser3.CommonParsers.{commentStart, eol, stringParser, syntaxStart, withProv}
import scitzen.sast.MacroCommand.Comment
import scitzen.sast.{Attribute, Inline, InlineText, Macro}

object InlineParsers:

  def full(ending: P0[Unit], allowEmpty: Boolean): P[(List[Inline], String)] =
    val comment: P[Macro] =
      (withProv(commentStart *> (until0(eol) ~ (peek(ending) | eol)).string))
        .map { (text, prov) => Macro(Comment, Attribute("", text).toAttributes, prov) }

    val notSyntax: P[String] =
      until(syntaxStart | ending)

    val simpleText: P[InlineText] = {
      notSyntax.string.map(InlineText)
    }

    val base = (comment | scitzen.parser3.MacroParsers.full | simpleText)
    if (allowEmpty&& ending.isInstanceOf[P[Unit]])
      base.rep0.map(_.toList).with1 ~ ending.asInstanceOf[P[Unit]].string
    else base.rep.map(_.toList) ~ ending.string


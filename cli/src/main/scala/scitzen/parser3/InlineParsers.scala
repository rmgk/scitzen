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

  transparent inline def full(ending: P0[Unit], allowEmpty: Boolean): P0[(List[Inline], String)] =
    val comment: P[Macro] =
      (withProv(commentStart *> (until0(eol) ~ (peek(ending) | eol)).string))
        .map { (text, prov) => Macro(Comment, Attribute("", text).toAttributes)( prov) }

    val notSyntax: P[String] =
      until(syntaxStart | ending).withContext("not syntax")

    val simpleText: P[InlineText] = {
      notSyntax.string.map(InlineText).withContext("simple text")
    }

    val base = (comment | MacroParsers.full | simpleText).withContext("base")
    if allowEmpty then
      (base.rep0.map(_.toList) ~ ending.string).withContext("inlines")
    else (base.rep.map(_.toList) ~ ending.string).withContext("inlines")

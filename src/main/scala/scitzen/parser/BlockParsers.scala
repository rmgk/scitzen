package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.generic.Sast
import scitzen.generic.Sast.{Paragraph, SBlock, SMacro, Section, SpaceComment, Text}
import scitzen.parser.CommonParsers._

object BlockParsers {

  def paragraph[_: P]: P[SBlock] =
    P((
      (AttributesParser.braces ~ spaceLine).? ~
        (withProv((untilE(eol ~ spaceLine) ~ eol).!) ~ spaceLine)
    ).map {
      case (attrOpt, (text, prov)) =>
        val inlines = Parse.valueOrThrow(Parse.paragraph(text, prov))
        SBlock(Attributes(attrOpt.getOrElse(Nil), prov), Paragraph(Text(inlines)))
    })

  def sectionStart[_: P]: P[String] = P(CharsWhileIn("=#").! ~ " ")
  def sectionTitle[_: P]: P[Section] =
    P(sectionStart
      ~ withProv(untilI(eol))
      ~ (AttributesParser.braces ~ spaceLine | AttributesParser.noBraces).?)
      .map {
        case (prefix, (str, prov), attrl) =>
          val inlines = Parse.valueOrThrow(Parse.paragraph(str, prov))
          Section(Text(inlines), prefix, Attributes(attrl.getOrElse(Nil), prov))
      }

  def extendedWhitespace[_: P]: P[SBlock] =
    P(withProv(
      (significantSpaceLine.rep(1) |
        MacroParsers.comment.rep(1)).rep(1).!
    )).map {
      case (str, prov) =>
        SBlock(Attributes(Nil, prov), SpaceComment(str))
    }

  def alternatives[_: P]: P[Sast] =
    P(extendedWhitespace |
      ListParsers.list |
      DelimitedBlockParsers.anyDelimited |
      sectionTitle |
      MacroParsers.full.map(SMacro.apply) ~ spaceLine |
      paragraph)

}

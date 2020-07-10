package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object BlockParsers {

  def paragraph[_: P]: P[NormalBlock] =
    P((
      (AttributesParser.braces ~ spaceLine).? ~
      (withProv((untilE(eol ~ spaceLine) ~ eol).!) ~ spaceLine))
      .map {case (attrOpt, (text, prov)) => NormalBlock("", BlockCommand(""), text, Attributes(attrOpt.getOrElse(Nil), prov))
      })

  def sectionStart[_: P]: P[String] = P(CharsWhileIn("=#").! ~ " ")
  def sectionTitle[_: P]: P[SectionTitle] =
    P(sectionStart ~ withProv(untilI(eol)) ~
      (AttributesParser.braces ~ spaceLine | AttributesParser.noBraces).?)
    .map { case (prefix, (str, prov), attrl) =>
      SectionTitle(prefix, str, Attributes(attrl.getOrElse(Nil), prov))
    }

  def extendedWhitespace[_: P]: P[WhitespaceBlock] =
    P(withProv(
      (significantSpaceLine.rep(1) |
       MacroParsers.comment.rep(1)
      ).rep(1).!))
    .map(WhitespaceBlock.apply.tupled)

  def alternatives[_: P]: P[BlockContent] = P(extendedWhitespace |
                                              ListParsers.list |
                                              DelimitedBlockParsers.full |
                                              sectionTitle |
                                              MacroParsers.full ~ spaceLine |
                                              paragraph)

}

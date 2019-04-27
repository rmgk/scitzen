package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object BlockParsers {

  // \ to escape newlines, + \ to escape newlines but keep newlines
  private def titleLine[_: P] = untilI(eol)

  def whitespaceBlock[_:P]: P[WhitespaceBlock] = P(significantSpaceLine.rep(1).!).map(WhitespaceBlock.apply)

  def paragraph[_:P]: P[NormalBlock] = P(untilE(eol ~ (spaceLine | DelimitedBlockParsers.anyStart.map(_ => ()))) ~ eol)
                                          .map(NormalBlock("", _))

  def sectionTitle[_:P]: P[SectionTitle] = P("=".rep(1).! ~ " " ~ titleLine)
                                           .map { case (level, str) => SectionTitle(level.length, str) }

  def commentBlock[_:P]: P[NormalBlock] =
    P((DelimitedBlockParsers.makeDelimited("/".rep(4).!)
       | (":%" ~ untilI(eol))
      ).rep(1).!)
    .map(NormalBlock("", _))

  def extendedWhitespace[_: P]: P[WhitespaceBlock] = P((whitespaceBlock | commentBlock).rep(1).!)
                                                     .map(WhitespaceBlock.apply)

  def alternatives[_: P]: P[BlockContent] = P(extendedWhitespace |
                                              AttributeBlockParser.entry |
                                              ListParsers.list |
                                              DelimitedBlockParsers.full |
                                              sectionTitle |
                                              MacroParsers.full ~ spaceLine |
                                              paragraph)

  def fullBlock[_: P]: P[Block] = P(Index ~ Attributes.line.rep ~ alternatives ~ Index).map {
    case (start, attrs, content, end) => Block(attrs, Prov(start, end), content)
  }
}

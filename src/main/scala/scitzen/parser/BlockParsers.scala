package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object BlockParsers {

  // \ to escape newlines, + \ to escape newlines but keep newlines
  private def titleLine[_: P] = untilI(eol)

  def blockTitle[_:P]: P[String] = P("." ~ !(" " | "...") ~ titleLine)

  def horizontalRule[_:P]: P[BlockMacro] = P(("'''" | "---" | "- - -" | "***" | "* * *").! ~ spaceLine)
                                           .map(BlockMacro.apply("horizontal-rule", _, Nil))
  def pageBreak     [_:P]: P[BlockMacro] = P("<<<".!).map(BlockMacro.apply("page-break", _, Nil))

  def whitespaceBlock[_:P]: P[NormalBlock] = P(significantSpaceLine.rep(1).!).map(NormalBlock(BlockType.Whitespace, _))

  def paragraph[_:P]: P[NormalBlock] = P(untilE(eol ~ (spaceLine | DelimitedBlockParsers.anyStart.map(_ => ()))) ~ eol)
                                       .map(NormalBlock(BlockType.Paragraph, _))

  def sectionTitle[_:P]: P[SectionTitle] = P("=".rep(2).! ~ " " ~ titleLine)
                                           .map { case (level, str) => SectionTitle(level.length - 1, str) }

  def commentBlock[_:P]: P[NormalBlock] =
    P((DelimitedBlockParsers.makeDelimited("/".rep(4).!)
       | ("//" ~ untilI(eol))
      ).rep(1).!)
    .map(NormalBlock(BlockType.Paragraph, _))

  def extendedWhitespace[_:P]: P[NormalBlock] = P((whitespaceBlock | commentBlock).rep(1).!)
                                                .map(NormalBlock(BlockType.Whitespace, _))

  def alternatives[_:P]: P[Block] = P(extendedWhitespace |
                                      ListParsers.list |
                                      DelimitedBlockParsers.full |
                                      horizontalRule |
                                      sectionTitle |
                                      MacroParsers.block |
                                      paragraph)

  def fullBlock[_:P]: P[Block] = P(Attributes.line.rep ~ blockTitle.? ~ Attributes.line.rep ~ alternatives)
                                 .map {
                                   case (Nil, None, Nil, content)         => content
                                   case (attrs1, stitle, attrs2, content) =>
                                     BlockWithAttributes(content, attrs1 ++ attrs2, stitle)
                                 }
}

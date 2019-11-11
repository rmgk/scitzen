package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.parser.MacroCommand.Other

object BlockParsers {


  def paragraph[_: P]: P[NormalBlock] =
    P(withProv(
      (untilE(eol ~ spaceLine) ~ eol).! ~ spaceLine)
      .map {NormalBlock("", _)})

  def sectionStart[_: P]: P[Int] = P("=".rep(1).! ~ " ").map(_.length)

  def sectionTitle[_:P]: P[SectionTitle] = P(sectionStart ~ untilI(eol))
                                           .map { case (level, str) => SectionTitle(level, str) }

  def horizontalRuleChars[_: P] = P(AnyChar("'\\-*"))
  def horizontalRule[_: P]: P[Macro] = P(Index ~ (verticalSpaces ~ horizontalRuleChars.!.flatMap { chr =>
    (verticalSpace ~ chr).rep(2) ~ spaceLine
  }).! ~ Index).map{case (s, text, e) => Macro(Other("horizontal-rule"), Attributes(List(List(Attribute("", text.dropRight(1)))), Prov(s, e)))}

  def whitespaceBlock[_:P]: P[String] = P(significantSpaceLine.rep(1).!)
  def commentBlock[_:P]: P[String] =
    P((DelimitedBlockParsers.makeDelimited("/".rep(4).!)
       | (":%" ~ untilI(eol))
      ).rep(1).!)
  def extendedWhitespace[_: P]: P[WhitespaceBlock] = P((whitespaceBlock | commentBlock).rep(1).!)
                                                     .map(WhitespaceBlock.apply)

  def alternatives[_: P]: P[BlockContent] = P(extendedWhitespace |
                                              AttributeBlockParser.entry |
                                              horizontalRule |
                                              ListParsers.list |
                                              DelimitedBlockParsers.full |
                                              sectionTitle |
                                              MacroParsers.full ~ spaceLine |
                                              paragraph)

  def fullBlock[_: P]: P[Block] = P(withProv(AttributesParser.line.rep ~ alternatives)).map {
    case ((attrs, content), prov) => Block(attrs, prov, content)
  }
}

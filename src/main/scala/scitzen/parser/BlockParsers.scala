package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.parser.MacroCommand.Other

object BlockParsers {


  def paragraph[_: P]: P[NormalBlock] =
    P(withProv(
      ((untilE(eol ~ spaceLine) ~ eol).! ~ spaceLine))
      .map {NormalBlock("", _)})

  def sectionStart[_: P]: P[(Int, Seq[Attribute])] = P("=".rep(1).! ~ AttributesParser.braces.? ~ " ")
  .map {
    case (e, attr) => (e.length, attr.getOrElse(Nil))
  }

  def sectionTitle[_: P]: P[SectionTitle] =
    P(withProv(sectionStart ~ untilI(eol) ~
      (AttributesParser.braces ~ spaceLine | AttributesParser.noBraces).?))
    .map { case ((level, pattr, str, attrl), prov) =>
      SectionTitle(level, str, Attributes(List(pattr, attrl.getOrElse(Nil)).flatten, prov))
    }

  def horizontalRuleChars[_: P] = P(AnyChar("'\\-*"))
  def horizontalRule[_: P]: P[Macro] = P(Index ~ (verticalSpaces ~ horizontalRuleChars.!.flatMap { chr =>
    (verticalSpace ~ chr).rep(2) ~ spaceLine
  }).! ~ Index).map { case (s, text, e) => Macro(Other("horizontal-rule"), Attribute("", text.dropRight(1)).toAttributes(Prov(s, e))) }

  def whitespaceBlock[_: P]: P[String] = P(significantSpaceLine.rep(1).!)
  def commentBlock[_: P]: P[String] =
    P((DelimitedBlockParsers.makeDelimited("/".rep(4).!)
       | (":%" ~ untilI(eol))
      ).rep(1).!)
  def extendedWhitespace[_: P]: P[WhitespaceBlock] = P(withProv((whitespaceBlock | commentBlock).rep(1).!))
  .map(WhitespaceBlock.apply.tupled)

  def alternatives[_: P]: P[BlockContent] = P(extendedWhitespace |
                                              horizontalRule |
                                              ListParsers.list |
                                              DelimitedBlockParsers.full |
                                              sectionTitle |
                                              MacroParsers.full ~ spaceLine |
                                              paragraph)

  def fullBlock[_: P]: P[BlockContent] = alternatives
}

package asciimedic

import asciimedic.CommonParsers._
import fastparse.all._

object BlockParsers {

  val blockTitle: Parser[String] = P("." ~ !(" " | "...") ~ InlineParser.titleLine)

  val horizontalRule: Parser[BlockMacro] = P(("'''" | "---" | "- - -" | "***" | "* * *").!)
                                           .map(BlockMacro.apply("horizontal-rule", _, Nil))
  val pageBreak     : Parser[BlockMacro] = P("<<<".!).map(BlockMacro.apply("page-break", _, Nil))

  val whitespaceBlock: Parser[NormalBlock] = P(swsLine.rep(min = 1).!).map(NormalBlock(BlockType.Whitespace, _))

  val paragraph: Parser[NormalBlock] = P(untilI(End | newline ~ iwsLine))
                                       .map(NormalBlock(BlockType.Paragraph, _))

  val sectionTitle: Parser[SectionTitle] = P("=".rep(2).! ~ " " ~ InlineParser.titleLine)
                                           .map { case (level, str) => SectionTitle(level.length - 1, str) }

  val commentBlock: Parser[NormalBlock] =
    P((DelimitedBlockParsers.makeDelimited("/".rep(min = 4).!)
       | ("//" ~ untilI(eol))
      ).rep(min = 1).!)
    .map(NormalBlock(BlockType.Paragraph, _))

  val extendedWhitespace: Parser[NormalBlock] = P((whitespaceBlock | commentBlock).rep(min = 1).!)
                                                .map(NormalBlock(BlockType.Whitespace, _))

  val alternatives: Parser[Block] = P(extendedWhitespace |
                                      ListParsers.list |
                                      DelimitedBlockParsers.full |
                                      horizontalRule |
                                      sectionTitle |
                                      MacroParsers.block |
                                      paragraph)

  val fullBlock: Parser[Block] = P(Attributes.line.rep ~ blockTitle.? ~ Attributes.line.rep ~ alternatives)
                                 .map {
                                   case (Nil, None, Nil, content)         => content
                                   case (attrs1, stitle, attrs2, content) =>
                                     BlockWithAttributes(content, attrs1 ++ attrs2, stitle)
                                 }
}

package asciimedic

import asciimedic.CommonParsers._
import fastparse.all._

object BlockParsers {

  val blockTitle: Parser[String] = P("." ~ !(" " | "...") ~ InlineParser.titleLine)

  val horizontalRule: Parser[BlockMacro] = P(("'''" | "---" | "- - -" | "***" | "* * *").!)
                                           .map(BlockMacro.apply("horizontal-rule", _, Nil))
  val pageBreak     : Parser[BlockMacro] = P("<<<".!).map(BlockMacro.apply("page-break", _, Nil))

  val whitespaceBlock: Parser[NormalBlock] = P((swsLine | ("//" ~ untilI(eol))).rep(min = 1).!)
                                             .map(NormalBlock(BlockType.Whitespace, _))

  val paragraph: Parser[NormalBlock] = P(untilI(End | newlineCharacter ~ iwsLine)).map(NormalBlock(BlockType.Paragraph, _))

  val sectionTitle: Parser[SectionTitle] = P("=".rep(2).! ~ " " ~ InlineParser.titleLine)
                                           .map { case (level, str) => SectionTitle(level.length - 1, str) }

  val alternatives: Parser[Block] = P(whitespaceBlock |
                                      Lists.list |
                                      Delimited.full |
                                      horizontalRule |
                                      sectionTitle |
                                      Macros.block |
                                      paragraph).log()

  val fullBlock: Parser[Block] = P(Attributes.line.rep ~ blockTitle.? ~ Attributes.line.rep ~ alternatives).log()
                                 .map {
                               case (Nil, None, Nil, content)         => content
                               case (attrs1, stitle, attrs2, content) =>
                                 BlockWithAttributes(content, attrs1 ++ attrs2, stitle)
                             }

  object Delimited {
    val normalDelimiters         = "/=-.+_*"
    val normalStart              = P(normalDelimiters.map(c => c.toString.rep(4)).reduce(_ | _))
    val anyStart: Parser[String] = P((normalStart | "--" | "```" | ("|" ~ "=".rep(3))).! ~ iwsLine)

    val full: Parser[NormalBlock] = P(
      (anyStart ~/ Pass).flatMap { delimiter =>
        untilI(eol ~ delimiter, min = 0).map(content => NormalBlock(BlockType.Delimited(delimiter), content))
      }
    )
  }

  object Lists {
    val listItemMarker = P("*".rep(1) ~ " ")
    val listContent    = P(untilE(eol ~ (iwsLine | listItemMarker)))
    val listItem       = P((listItemMarker.! ~/ listContent.!)
                           .map((ListItem.apply _).tupled)).log()
    val list           = P(listItem.rep(1, sep = aws ~ Pass)
                           .map(ListBlock) ~ aws)
  }
}

package scitzen.parser

import de.rmgk.scip.*
import scitzen.parser.CommonParsers.*
import scitzen.sast.{Attributes, BCommand, Block, Inline, Paragraph, Sast, Section, SpaceComment, Text}

object BlockParsers {

  val sectionInlines: Scip[List[Inline]]   = InlineParsers.full(eol)
  val paragraphInlines: Scip[List[Inline]] = InlineParsers.full(eol and spaceLineB)

  def paragraph: Scip[Block] = Scip {
    val attrOpt         = (AttributesParser.braces <~ spaceLineF).opt.run
    val (inlines, prov) = withProv(paragraphInlines).run
    Block(BCommand.Empty, scitzen.sast.Attributes(attrOpt.getOrElse(Nil)), Paragraph(Text(inlines)))(prov)
  }

  def sectionStart: Scip[String] = choice("= ", "== ", "# ", "## ", "### ").str
  def sectionTitle: Scip[Section] = Scip {
    val prefix          = sectionStart.run
    val (inlines, prov) = withProv(sectionInlines).run
    val attrl           = ((AttributesParser.braces <~ spaceLineF) | AttributesParser.noBraces).opt.run
    Section(scitzen.sast.Text(inlines), prefix.substring(0, prefix.length - 1), Attributes(attrl.getOrElse(Nil)))(prov)
  }

  val extendedWhitespace: Scip[Block] = Scip {
    val (str, prov) = withProv(
      significantSpaceLine.trace("spaceline").or(
        DirectiveParsers.comment.attempt.trace("comment")
      ).trace("space or comment").rep.min(1).str
    ).run
    Block(BCommand.Empty, scitzen.sast.Attributes(Nil), SpaceComment(str))(prov)
  }

  def alternatives: Scip[Sast] =
    (extendedWhitespace.trace("block ws") |
      ListParsers.list.trace("block list") |
      DelimitedBlockParsers.anyDelimited.trace("block delim") |
      sectionTitle.trace("block title") |
      (DirectiveParsers.full <~ spaceLineF).trace("block directive") |
      paragraph.trace("block para")).trace("block")

}

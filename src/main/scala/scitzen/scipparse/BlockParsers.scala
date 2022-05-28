package scitzen.scipparse

import de.rmgk.scip.*
import scitzen.sast.{Attributes, Block, Paragraph, Sast, Section, SpaceComment, Text}
import scitzen.scipparse.CommonParsers.*

object BlockParsers {

  val paragraphInlines = InlineParsers(
    "\n".any,
    eolB and spaceLineB
  )

  val sectionInlines = InlineParsers("\n".any, eolB)

  def paragraph: Scip[Block] = Scip {
    val attrOpt              = (AttributesParser.braces <~ spaceLineF).opt.run
    val ((inlines, _), prov) = withProv(paragraphInlines.full).run
    Block(scitzen.sast.Attributes(attrOpt.getOrElse(Nil)), Paragraph(Text(inlines)), prov)
  }

  def sectionStart: Scip[String] = "=#".any.rep.min(1).str <~ " ".all
  def sectionTitle: Scip[Section] = Scip {
    val prefix               = sectionStart.run
    val ((inlines, _), prov) = withProv(sectionInlines.full).run
    val attrl                = choice(AttributesParser.braces <~ spaceLineF, AttributesParser.noBraces).opt.run
    Section(scitzen.sast.Text(inlines), prefix, Attributes(attrl.getOrElse(Nil)))(prov)
  }

  val extendedWhitespace: Scip[Block] = Scip {
    val (str, prov) = withProv(
      (significantSpaceLineB.trace("spaceline").or(
      DirectiveParsers.comment.attempt.trace("comment")))
        .trace("space or comment").rep.min(1).str
    ).run
    Block(scitzen.sast.Attributes(Nil), SpaceComment(str), prov)
  }

  def alternatives: Scip[Sast] =
    choice(
      extendedWhitespace.trace("block ws"),
      ListParsers.list.trace("block list"),
      DelimitedBlockParsers.anyDelimited.trace("block delim"),
      sectionTitle.trace("block title"),
      (DirectiveParsers.full <~ spaceLineF).trace("block directive"),
      paragraph.trace("block para")
      ).trace("block")

}

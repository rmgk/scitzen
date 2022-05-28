package scitzen.scipparse

import de.rmgk.scip.*
import scitzen.sast.{Attributes, Block, Paragraph, Sast, Section, SpaceComment, Text}
import scitzen.scipparse.CommonParsers.*
import scitzen.scipparse.CompatParsers.*

object BlockParsers {

  val paragraphInlines = InlineParsers(
    "\n".any,
    eol ~> spaceLine
  )

  val sectionInlines = InlineParsers("\n".any, eol)

  def paragraph: Scip[Block] = Scip {
    val attrOpt              = (AttributesParser.braces <~ spaceLine).opt.run
    val ((inlines, _), prov) = withProv(paragraphInlines.full).run
    Block(scitzen.sast.Attributes(attrOpt.getOrElse(Nil)), Paragraph(Text(inlines)), prov)
  }

  def sectionStart: Scip[String] = "=#".any.rep.min(1).orFail.str <~ " ".scip
  def sectionTitle: Scip[Section] = Scip {
    val prefix               = sectionStart.run
    val ((inlines, _), prov) = withProv(sectionInlines.full).run
    val attrl                = choice(AttributesParser.braces <~ spaceLine, AttributesParser.noBraces).opt.run
    Section(scitzen.sast.Text(inlines), prefix, Attributes(attrl.getOrElse(Nil)))(prov)
  }

  val extendedWhitespace: Scip[Block] = Scip {
    val (str, prov) = withProv(
      choice(
        significantSpaceLine.attempt.trace("spaceline").rep.min(1).orFail,
        DirectiveParsers.comment.attempt.trace("comment").rep.min(1).orFail
      ).trace("space or comment").attempt.rep.min(1).orFail.str
    ).run
    Block(scitzen.sast.Attributes(Nil), SpaceComment(str), prov)
  }

  def alternatives: Scip[Sast] =
    choice(
      extendedWhitespace.trace("block ws"),
      ListParsers.list.trace("block list"),
      DelimitedBlockParsers.anyDelimited.trace("block delim"),
      sectionTitle.trace("block title"),
      (DirectiveParsers.full <~ spaceLine).trace("block directive"),
      paragraph.trace("block para")
    ).trace("block")

}

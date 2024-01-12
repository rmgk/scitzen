package scitzen.parser

import de.rmgk.scip.{Scip, all, any, choice, scx, seq}
import scitzen.parser.CommonParsers.{eol, newline, spaceLineF, untilIS}
import scitzen.parser.{AttributesParser, CommonParsers, DelimitedBlockParsers, DirectiveParsers}
import scitzen.sast.{
  Atom, Attribute, Attributes, BCommand, DefinitionListAtom, Delimiter, Directive, Fenced, FusedDelimited, Inline,
  InlineText, ListAtom, Meta, Prov, Section, SpaceComment, Text, TextAtom
}

import java.awt.image.ColorModel

object AtomParsers {

  def alternatives: Scip[Atom] = Scip {
    (
      (AtomParsers.fenced: Scip[Atom]) |
      AtomParsers.whitespace |
      annotatedAtom(
        AtomParsers.section.trace("section") |
        AtomParsers.definitionList.trace("definition list") |
        AtomParsers.list.trace("list") |
        AtomParsers.delimited.trace("block delim") |
        (DirectiveParsers.full <~ CommonParsers.spaceLineF).trace("block directive").map(dir =>
          (meta: Meta) => dir.copy(meta = meta)
        ) |
        AtomParsers.unquoted
      )
    ).trace("block").run
  }

  inline def annotatedAtom[Atom](inline atomParser: Scip[Meta => Atom]): Scip[Atom] =
    Scip {
      val start  = scx.index
      val indent = CommonParsers.verticalSpaces.str.run
      val atom   = atomParser.run
      val end    = scx.index
      atom(Meta(indent, Prov(start, end)))
    }

  val textline: Scip[List[Inline]] = Scip {
    val inlines = InlineParsers.full(eol).run
    if inlines.isEmpty then scx.fail
    else inlines
  }

  def section: Scip[Meta => Section] =
    Scip {
      val marker  = choice("= ", "== ", "# ", "## ", "### ").str.run
      val inlines = textline.run
      val attrl   = AttributesParser.namedAttribute.list(eol).run
      meta => Section(Text(inlines), marker.substring(0, marker.length - 1), Attributes(attrl), meta)
    }

  def definitionList: Scip[Meta => DefinitionListAtom] = Scip {
    val marker  = ("-•*".any and ": ".all).str.run
    val inlines = textline.run
    meta => DefinitionListAtom(marker, inlines, meta)
  }

  def list: Scip[Meta => (ListAtom | DefinitionListAtom)] =
    Scip {
      val marker  = (("-•*".any or (CommonParsers.digits and ".".all)) and " ".all).str.run
      val inlines = textline.run
      meta => ListAtom(s"$marker", inlines, meta)
    }

  def unquoted: Scip[Meta => TextAtom] = textline.map(t => (meta: Meta) => TextAtom(Text(t), meta))

  def whitespace: Scip[SpaceComment] = Scip {
    val start = scx.index
    val content = (
      CommonParsers.significantSpaceLine or
        (CommonParsers.verticalSpaces and DirectiveParsers.commentContent.attempt and eol)
    ).rep.min(1).str.run
    val prov = Prov(start, scx.index)
    SpaceComment(content, Meta("", prov))
  }

  def delimited: Scip[Meta => Delimiter] = Scip {
    val start   = ":".any.rep.min(2).str.run
    val command = DirectiveParsers.macroCommand.opt.trace("delimited marco").run
    val attr    = (AttributesParser.braces.opt <~ CommonParsers.spaceLineF).trace("delim braces").run
    meta =>
      Delimiter(
        marker = start,
        command = BCommand.parse(command.getOrElse("")),
        attributes = Attributes(attr.getOrElse(Nil)),
        meta
      )
  }

  def fenced: Scip[Fenced] = Scip {
    val sindex  = scx.index
    val indent  = CommonParsers.verticalSpaces.str.run
    val start   = "`".any.rep.min(2).str.run
    val command = DirectiveParsers.macroCommand.opt.trace("fenced macro").run
    val attr    = (AttributesParser.braces.opt <~ CommonParsers.spaceLineF).trace("fenced braces").run
    val content = untilIS(newline and seq(indent) and seq(start) and eol).run

    val innerContent = content.linesWithSeparators.map(line => line.stripPrefix(indent)).mkString

    val strippedContent = DelimitedBlockParsers.stripIfPossible(
      innerContent,
      start.length
    ).getOrElse(innerContent)

    val prov = Prov(sindex, scx.index)

    Fenced(
      command = BCommand.parse(command.getOrElse("")),
      attributes = Attributes(attr.getOrElse(Nil)),
      content = strippedContent,
      meta = Meta(indent, prov)
    )
  }
}

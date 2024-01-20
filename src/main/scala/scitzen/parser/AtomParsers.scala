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
      annotatedAtom((indent, start) =>
        AtomParsers.section(indent, start).trace("section") |
        AtomParsers.definitionList(indent, start).trace("definition list") |
        AtomParsers.list(indent, start).trace("list") |
        AtomParsers.delimited(indent, start).trace("block delim") |
        (DirectiveParsers.full <~ CommonParsers.spaceLineF).trace("block directive").map(dir =>
          dir.copy(meta = Meta(indent, dir.meta.prov))
        ) |
        AtomParsers.unquoted(indent, start)
      )
    ).trace("block").run
  }

  inline def annotatedAtom[Atom](inline atomParser: (String, Int) => Scip[Atom]): Scip[Atom] =
    Scip {
      val indent = CommonParsers.verticalSpaces.str.run
      val start  = scx.index
      atomParser(indent, start).run
    }

  val textline: Scip[List[Inline]] = Scip {
    val inlines = InlineParsers.full(eol).run
    if inlines.isEmpty then scx.fail
    else inlines
  }

  def section(indent: String, start: Int): Scip[Section] =
    Scip {
      val marker  = choice("= ", "== ", "# ", "## ", "### ").str.run
      val inlines = textline.run
      val attrl   = AttributesParser.namedAttribute.list(eol).run
      Section(
        Text(inlines),
        marker.substring(0, marker.length - 1),
        Attributes(attrl),
        Meta(indent, Prov(start, scx.index))
      )
    }

  def definitionList(indent: String, start: Int): Scip[DefinitionListAtom] = Scip {
    val marker  = ("-•*".any and ": ".all).str.run
    val inlines = textline.run
    DefinitionListAtom(marker, inlines, Meta(indent, Prov(start, scx.index)))
  }

  def list(indent: String, start: Int): Scip[ListAtom] =
    Scip {
      val marker  = (("-•*".any or (CommonParsers.digits and ".".all)) and " ".all).str.run
      val inlines = textline.run
      ListAtom(s"$marker", inlines, Meta(indent, Prov(start, scx.index)))
    }

  def unquoted(indent: String, start: Int): Scip[TextAtom] = Scip {
    val t = textline.run
    TextAtom(t, Meta(indent, Prov(start, scx.index)))
  }

  def whitespace: Scip[SpaceComment] = Scip {
    val start = scx.index
    val content = (
      CommonParsers.significantSpaceLine or
        (CommonParsers.verticalSpaces and DirectiveParsers.commentContent.attempt and eol)
    ).rep.min(1).str.run
    val prov = Prov(start, scx.index)
    SpaceComment(content, Meta("", prov))
  }

  def delimited(indent: String, start: Int): Scip[Delimiter] = Scip {
    val marker  = ":".any.rep.min(2).str.run
    val command = DirectiveParsers.macroCommand.opt.trace("delimited marco").run
    val attr    = (AttributesParser.braces.opt <~ CommonParsers.spaceLineF).trace("delim braces").run
    Delimiter(
      marker = marker,
      command = BCommand.parse(command.getOrElse("")),
      attributes = Attributes(attr.getOrElse(Nil)),
      meta = Meta(indent, Prov(start, scx.index))
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

package scitzen.parser

import de.rmgk.scip.{Scip, all, any, choice, scx, seq}
import scitzen.parser.CommonParsers.{eol, newline, spaceLineF, untilIS}
import scitzen.parser.{AttributesParser, CommonParsers, DelimitedBlockParsers, DirectiveParsers}
import scitzen.sast.{
  Attribute, Attributes, BCommand, Directive, Fenced, Inline, InlineText, Parsed, Prov, Section, SpaceComment, Text
}

import java.awt.image.ColorModel

object Atoms {

  def alternatives: Scip[Container[Atom]] = Scip {
    (
      Atoms.fenced |
      Atoms.whitespace |
      annotatedAtom(
        Atoms.section.trace("section") |
        Atoms.definitionList.trace("definition list") |
        Atoms.list.trace("list") |
        Atoms.delimited.trace("block delim") |
        (DirectiveParsers.full <~ CommonParsers.spaceLineF).trace("block directive") |
        Atoms.unquoted
      )
    ).trace("block").run
  }

  type Atom =
    Directive | Text | Delimiter | ListAtom | Section | SpaceComment | DefinitionListAtom | Fenced
  case class Container[+A <: Atom](indent: String, content: A, prov: Prov)
  object Container:
    def unapply[A <: Atom](cont: Container[A]): (String, A) = (cont.indent, cont.content)

  inline def annotatedAtom[A <: Atom](inline atomParser: Scip[A]): Scip[Container[A]] =
    Scip {
      val start  = scx.index
      val indent = CommonParsers.verticalSpaces.str.run
      val atom   = atomParser.run
      val end    = scx.index
      Container(indent, atom, Prov(start, end))
    }

  case class ListAtom(marker: String, content: Seq[Inline])
  case class DefinitionListAtom(marker: String, content: Seq[Inline])

  val textline: Scip[List[Inline]] = Scip {
    val inlines = InlineParsers.full(eol).run
    if inlines.isEmpty then scx.fail
    else inlines
  }

  def section: Scip[Section] =
    Scip {
      val marker  = choice("= ", "== ", "# ", "## ", "### ").str.run
      val inlines = textline.run
      val attrl   = AttributesParser.namedAttribute.list(eol).run
      Section(Text(inlines), marker.substring(0, marker.length - 1), Attributes(attrl))
    }

  def definitionList: Scip[DefinitionListAtom] = Scip {
    val marker  = ("-•*".any and ": ".all).str.run
    val inlines = textline.run
    DefinitionListAtom(marker, inlines)
  }

  def list: Scip[ListAtom | DefinitionListAtom] =
    Scip {
      val marker  = (("-•*".any or (CommonParsers.digits and ".".all)) and " ".all).str.run
      val inlines = textline.run
      ListAtom(s"$marker", inlines)
    }

  def unquoted: Scip[Text] = textline.map(Text.apply)

  def whitespace: Scip[Container[SpaceComment]] = Scip {
    val start = scx.index
    val content = (
      CommonParsers.significantSpaceLine or
        (CommonParsers.verticalSpaces and DirectiveParsers.commentContent.attempt and eol)
    ).rep.min(1).str.run
    val prov = Prov(start, scx.index)
    Container("", SpaceComment(content), prov)
  }

  case class Delimiter(marker: String, command: BCommand, attributes: Attributes)

  def delimited: Scip[Delimiter] = Scip {
    val start   = ":".any.rep.min(2).str.run
    val command = DirectiveParsers.macroCommand.opt.trace("delimited marco").run
    val attr    = (AttributesParser.braces.opt <~ CommonParsers.spaceLineF).trace("delim braces").run
    Delimiter(
      marker = start,
      command = BCommand.parse(command.getOrElse("")),
      attributes = Attributes(attr.getOrElse(Nil))
    )
  }

  def fenced: Scip[Container[Fenced]] = Scip {
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

    val fen = Fenced(
      command = BCommand.parse(command.getOrElse("")),
      attributes = Attributes(attr.getOrElse(Nil)),
      content = strippedContent,
      indent = indent,
      prov = prov,
    )
    Container(indent, fen, prov)
  }
}

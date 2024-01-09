package scitzen.parser

import de.rmgk.scip.{Scip, all, any, choice, scx, seq}
import scitzen.parser.CommonParsers.{eol, newline, spaceLineF, untilIS}
import scitzen.parser.{AttributesParser, CommonParsers, DelimitedBlockParsers, DirectiveParsers}
import scitzen.sast.{Attribute, Attributes, BCommand, Directive, Inline, Prov, Section, Text}

object Atoms {

  def alternatives: Scip[Container[Atom]] = Scip {

    Atoms.fenced.opt.run match
      case Some(res) => res
      case None =>
        annotatedAtom(
          Atoms.section.trace("section") |
          Atoms.list.trace("list") |
          Atoms.delimited.trace("block delim") |
          (DirectiveParsers.full <~ CommonParsers.spaceLineF).trace("block directive") |
          Atoms.whitespace |
          Atoms.unquoted
        ).trace("block").run
  }

  type Atom =
    Directive | Text | Whitespace | Delimited | Fenced | ListAtom | Section
  case class Container[+A <: Atom](indent: String, content: A)(val prov: Prov)

  def annotatedAtom[A <: Atom](atomParser: Scip[A]): Scip[Container[A]] = Scip {
    val start  = scx.index
    val indent = CommonParsers.verticalSpaces.str.run
    val atom   = atomParser.run
    val end    = scx.index
    Container(indent, atom)(Prov(start, end))
  }

  case class SectionAtom(prefix: String, content: Seq[Inline])
  case class ListAtom(prefix: String, content: Seq[Inline])

  val textline: Scip[List[Inline]] = Scip {
    val res = InlineParsers.full(eol).run
    if res.isEmpty then scx.fail
    else res
  }

  def section: Scip[Section] =
    Scip {
      val prefix  = choice("= ", "== ", "# ", "## ", "### ").str.run
      val inlines = textline.run
      val attrl   = AttributesParser.namedAttribute.list(eol).run
      // need to eat one newnline …
      if attrl.nonEmpty then eol.orFail.run
      Section(Text(inlines), prefix.substring(0, prefix.length - 1), Attributes(attrl))
    }

  def list: Scip[ListAtom] =
    Scip {
      val prefix  = (("-•*".any or (CommonParsers.digits and ".".all)) and " ".all).str.run
      val inlines = textline.run
      ListAtom(prefix, inlines)
    }

  def unquoted: Scip[Text] = textline.map(Text.apply)

  case class Whitespace(content: String):
    override def toString: String = s"Whitespace(»${content.replace("\n", "\\n")}«)"

  def whitespace: Scip[Whitespace] =
    (CommonParsers.verticalSpaces and (DirectiveParsers.commentContent.attempt or Scip(true)) and eol).str.map(
      Whitespace.apply
    )

  case class Delimited(delimiter: String, command: BCommand, attributes: Attributes)

  def delimited: Scip[Delimited] = Scip {
    val start   = ":".any.rep.min(2).str.run
    val command = DirectiveParsers.macroCommand.opt.trace("delimited marco").run
    val attr    = (AttributesParser.braces.opt <~ CommonParsers.spaceLineF).trace("delim braces").run
    Delimited(
      delimiter = start,
      command = BCommand.parse(command.getOrElse("")),
      attributes = Attributes(attr.getOrElse(Nil))
    )
  }

  case class Fenced(commands: BCommand, attributes: Attributes, content: String)

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
    val fen = Fenced(
      commands = BCommand.parse(command.getOrElse("")),
      attributes = Attributes(attr.getOrElse(Nil)),
      content = strippedContent
    )
    Container(indent, fen)(Prov(sindex, scx.index))
  }
}

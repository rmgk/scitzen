package scitzen.fusion

import scala.reflect.TypeTest
import de.rmgk.scip.{Scip, Scx, all, any, choice, scx, seq, until}
import scitzen.compat.Logging
import scitzen.fusion.Atoms.{Atom, Container, Delimited, KeyValue, ListAtom, SectionAtom, Whitespace, annotatedAtom}
import scitzen.parser.CommonParsers.{eol, newline, untilI, untilIS}
import scitzen.parser.ListParsers.ParsedListItem
import scitzen.parser.{
  AttributesParser, BlockParsers, CommonParsers, DelimitedBlockParsers, DirectiveParsers, ListParsers
}
import scitzen.project.{Document, Project}
import scitzen.sast.{
  Attribute, Attributes, BCommand, Block, Directive, Fenced, Inline, InlineText, Paragraph, Prov, Sast, Section,
  SpaceComment, Text
}

import java.nio.file.Path

object Fusion {

  def run(project: Project, absolute: List[Path]): Unit =
    absolute.map: abs =>
      val pp  = project.asProjectPath(abs)
      val doc = Document(pp)
      val res = documentUnwrap(doc)
      res.foreach(el => println(el))
    ()

  def documentUnwrap(doc: Document): List[Sast] = {
    val content = doc.content
    def newscx() = Scx(
      input = content,
      index = 0,
      maxpos = content.length,
      depth = 0,
      lastFail = -1,
      tracing = false
    )

    val scx = newscx()

    def rec(fuser: Fuser): Fuser =
      if scx.index >= scx.maxpos
      then fuser
      else
        val atom = Atoms.alternatives.runInContext(scx)
        fuser.add(atom) match
          case Some(fuser) => rec(fuser)
          case None        => ???

    rec(TopFuser(Nil)).close().reverse

  }

  trait Fuser {
    def add(container: Container[Atom]): Option[Fuser]
    def close(): List[Sast]

    def unhandled(container: Container[Atom]): Unit =
      Logging.cli.warn(s"received $container, (${container.prov}) not handled by $this")
  }

  case class TopFuser(sastAcc: List[Sast]) extends Fuser {

    override def close(): List[Sast] = sastAcc

    def add(container: Container[Atom]): Option[Fuser] =
      container.content match
        case kv: KeyValue =>
          Logging.cli.warn(s"line looking like key value pair: »${kv}« reinterpreted as text")
          add(container.copy(content = Text(InlineText(kv.indent + kv.attribute.id) +: kv.attribute.text.inl))(
            container.prov
          ))
        case dir: Directive => Some(TopFuser(dir :: sastAcc))
        case Atoms.Fenced(commands, attributes, content) =>
          val block = Block(commands, attributes, Fenced(content))(container.prov)
          Some(TopFuser(block :: sastAcc))
        case del: Delimited =>
          Some(StackFuser(BlockFuser(del, container.prov, TopFuser(Nil), done = false), this))
        case other =>
          LazyList(SectionFuser, ParagraphFuser, ListFuser, WhitespaceFuser).flatMap(_.add(container)).headOption match
            case None =>
              unhandled(container)
              Some(this)
            case Some(child) =>
              Some(StackFuser(child, this))
  }

  case class StackFuser(current: Fuser, top: TopFuser) extends Fuser {
    override def close(): List[Sast] = current.close() ::: top.close()
    def add(container: Container[Atom]): Option[Fuser] =
      current.add(container) match
        case None      => top.copy(sastAcc = close()).add(container)
        case Some(res) => Some(copy(current = res))

  }

  case class BlockFuser(delimited: Delimited, prov: Prov, current: Fuser, done: Boolean) extends Fuser {
    override def close(): List[Sast] =
      val inner = current.close().reverse
      List(Block(delimited.command, delimited.attributes, scitzen.sast.Parsed(delimited.delimiter, inner))(prov))
    def add(container: Container[Atom]): Option[Fuser] =
      if done then None
      else
        container.content match
          case Delimited(delimited.delimiter, BCommand.Empty, Attributes.empty) =>
            Some(copy(done = true, prov = prov.copy(end = container.prov.end)))
          case other =>
            current.add(container) match
              case Some(res) =>
                Some(copy(current = res))
              case None =>
                unhandled(container)
                None
  }

  val SectionFuser = AccumulatingFuser[KeyValue | SectionAtom](
    Nil,
    {
      containers =>
        containers.reverse match
          case (head @ Container(_, SectionAtom(pfx, content))) :: keyValues =>
            val attributes = Attributes(keyValues.map {
              case Container(_, kv: KeyValue) => kv.attribute
              case other                      => ???
            }.toSeq)
            List(Section(Text(content), pfx, attributes)(head.prov))
          case other =>
            println(s"head was not a container but: $other")
            ???

    }
  )

  def combineProvidence(containers: Seq[Container[Atom]]): Prov =
    Prov(
      containers.iterator.map(_.prov.start).min,
      containers.iterator.map(_.prov.end).max
    )

  val ParagraphFuser = AccumulatingFuser[Text](
    Nil,
    { containers =>
      val text = Text(containers.reverseIterator.flatMap(c => InlineText("\n") +: c.content.inl).toSeq.drop(1))
      List(Block(BCommand.Empty, Attributes.empty, Paragraph(text))(combineProvidence(containers)))
    }
  )

  val WhitespaceFuser = AccumulatingFuser[Whitespace](
    Nil,
    { containers =>
      List(Block(
        BCommand.Empty,
        Attributes.empty,
        SpaceComment(containers.reverseIterator.map(c => c.content.content).mkString)
      )(combineProvidence(containers)))
    }
  )

  case class AccumulatingFuser[As <: Atom](
      accumulator: List[Container[As]],
      combiner: List[Container[As]] => List[Sast],
  )(using TypeTest[Atom, As]) extends Fuser {
    override def add(container: Container[Atom]): Option[Fuser] = container.content match
      case _: As =>
        Some(copy(accumulator = container.asInstanceOf[Container[As]] :: accumulator))
      case other => None
    override def close(): List[Sast] =
      combiner(accumulator)
  }

  val ListFuser = AccumulatingFuser[ListAtom | Text](
    Nil,
    { plis =>
      val combined = plis.foldRight(List.empty[ParsedListItem]): (cont, acc) =>
        cont match
          case Container(indent, ListAtom(prefix, content)) =>
            ParsedListItem(s"$indent$prefix", Text(content).plainString, cont.prov, None) :: acc
          case Container(indent, t: Text) =>
            val (curr :: res) = acc: @unchecked
            curr.copy(
              itemText = s"${curr.itemText}${t.plainString}",
              prov = Prov(curr.prov.start, cont.prov.end)
            ) :: res

      List(ListParsers.ListConverter.listtoSast(combined.reverse))
    }
  )

}

object Atoms {

  def alternatives: Scip[Container[Atom]] = Scip {

    Atoms.fenced.opt.run match
      case Some(res) => res
      case None =>
        annotatedAtom(
          Atoms.section.trace("section") |
          Atoms.list.trace("list") |
          Atoms.keyValue.trace("key value") |
          Atoms.delimited.trace("block delim") |
          (DirectiveParsers.full <~ CommonParsers.spaceLineF).trace("block directive") |
          Atoms.whitespace |
          Atoms.unquoted
        ).trace("block").run
  }

  type Atom =
    Directive | KeyValue | Text | Whitespace | Delimited | Fenced | ListAtom | SectionAtom
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

  def section: Scip[SectionAtom] =
    Scip {
      val prefix  = choice("= ", "== ", "# ", "## ", "### ").str.run
      val inlines = BlockParsers.sectionInlines.run
      SectionAtom(prefix.dropRight(1), inlines)
    }

  def list: Scip[ListAtom] =
    Scip {
      val prefix  = (("-•*".any or (CommonParsers.digits and ".".all)) and " ".all).str.run
      val inlines = BlockParsers.sectionInlines.run
      ListAtom(prefix, inlines)
    }

  case class KeyValue(indent: String, attribute: Attribute, prov: Prov)

  def keyValue: Scip[KeyValue] = Scip {
    val indent            = CommonParsers.verticalSpaces.str.run
    val (attribute, prov) = CommonParsers.withProv(AttributesParser.namedAttribute).run
    CommonParsers.eol.run
    KeyValue(indent, attribute, prov)
  }

  def unquoted: Scip[Text] = BlockParsers.sectionInlines.map(Text.apply)

  case class Whitespace(content: String):
    override def toString: String = s"Whitespace(»${content.replace("\n", "\\n")}«)"

  def whitespace: Scip[Whitespace] =
    (CommonParsers.verticalSpaces and (DirectiveParsers.commentContent or eol)).str.map(Whitespace.apply)

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

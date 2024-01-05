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
  Attribute, Attributes, BCommand, Block, Directive, Fenced, Inline, InlineText, Paragraph, Prov, Sast, Section, Slist,
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

    def atoms(): Atoms =
      if scx.index >= scx.maxpos
      then LazyList.empty
      else
        val atom = Atoms.alternatives.runInContext(scx)
        atom #:: atoms()

    fuseTop(atoms(), Nil)
  }

  extension [A](list: LazyList[A])
    def collectWhile[R](f: A => Option[R]): (LazyList[R], LazyList[A]) =
      list match
        case LazyList() => (LazyList.empty, list)
        case head #:: tail =>
          f(head) match
            case None => (LazyList.empty, list)
            case Some(res) =>
              lazy val (good, bad) = tail.collectWhile(f)
              (res #:: good, bad)

  def collectType[As <: Atom](atoms: LazyList[Container[Atom]])(using
      TypeTest[Atom, As]
  ): (LazyList[Container[As]], LazyList[Container[Atom]]) =
    atoms.collectWhile:
      case cont @ Container(_, ws: As) => Some(cont.asInstanceOf[Container[As]])
      case other                       => None

  type Atoms = LazyList[Container[Atom]]

  def fuseTop(atoms: Atoms, sastAcc: List[Sast]): List[Sast] = {

    atoms match
      case LazyList() => sastAcc.reverse
      case container #:: tail =>
        container.content match
          case kv: KeyValue =>
            fuseTop(applyKVHack(container) #:: tail, sastAcc)
          case dir: Directive => fuseTop(tail, dir :: sastAcc)
          case Atoms.Fenced(commands, attributes, content) =>
            val block = Block(commands, attributes, Fenced(content))(container.prov)
            fuseTop(tail, block :: sastAcc)
          case del: Delimited =>
            val (delimited, rest) = fuseDelimited(container.indent, del, container.prov, tail)
            fuseTop(rest, delimited :: sastAcc)
          case SectionAtom(prefix, content) =>
            val (kvs, rest) = tail.collectWhile:
              case Container(indent, content: KeyValue) => Some(content)
              case other                                => None
            val attributes = Attributes(kvs.iterator.map { ckv => ckv.attribute }.toSeq)
            fuseTop(rest, Section(Text(content), prefix, attributes)(container.prov) :: sastAcc)
          case ListAtom(_, _) =>
            val (list, rest) = fuseList(atoms, Nil)
            fuseTop(rest, list :: sastAcc)
          case Whitespace(content) =>
            val (ws, rest) = collectType[Whitespace](atoms)
            val content    = ws.map(ws => s"${ws.indent}${ws.content.content}").mkString
            fuseTop(
              rest,
              Block(
                BCommand.Empty,
                Attributes.empty,
                SpaceComment(content)
              )(combineProvidence(ws)) :: sastAcc
            )
          case _: Text =>
            val (containers, rest) = collectType[Text](atoms)
            val text = Text(containers.iterator.flatMap(c =>
              InlineText("\n") +: ((if c.indent.nonEmpty then List(InlineText(c.indent)) else Nil) concat c.content.inl)
            ).drop(1).toSeq)
            fuseTop(
              rest,
              Block(BCommand.Empty, Attributes.empty, Paragraph(text))(combineProvidence(containers)) :: sastAcc
            )
  }

  def fuseList(atoms: Atoms, acc: List[ParsedListItem]): (Slist, Atoms) = {
    atoms match
      case (cont @ Container(indent, ListAtom(pfx, content))) #:: tail =>
        val (textSnippets, rest) = collectType[Text | Directive](tail)
        val snippets =
          textSnippets.flatMap { cont =>
            InlineText("\n") +: (
              (if cont.indent.nonEmpty
               then List(InlineText(cont.indent))
               else Nil)
              concat (
                cont.content match
                  case Text(inl) => inl
                  case dir: Directive => List(dir)
                )
            )
          }
        fuseList(rest, ParsedListItem(s"$indent$pfx", Text(content concat snippets), cont.prov, None) :: acc)
      case other =>
        (ListParsers.ListConverter.listtoSast(acc.reverse), atoms)
  }

  def fuseDelimited(indent: String, del: Delimited, prov: Prov, atoms: LazyList[Container[Atom]]) = {
    val (innerAtoms, rest) = atoms.span:
      case Container(`indent`, Delimited(del.delimiter, BCommand.Empty, Attributes.empty)) =>
        false
      case other => true
    val adaptedIndent = innerAtoms.map:
      case cont @ Container(cindent, content) => Container(cindent.stripPrefix(indent).stripPrefix("\t"), content)(cont.prov)
    val innerSast = fuseTop(adaptedIndent, Nil)
    (
      Block(del.command, del.attributes, scitzen.sast.Parsed(del.delimiter, innerSast))(prov),
      rest.drop(1)
    )

  }

  def combineProvidence(containers: Seq[Container[Atom]]): Prov =
    Prov(
      containers.iterator.map(_.prov.start).min,
      containers.iterator.map(_.prov.end).max
    )

  def applyKVHack(container: Container[Atom]): Container[Text] = {
    val kv = container.content.asInstanceOf[KeyValue]
    Logging.cli.warn(s"line looking like key value pair: »${kv}« reinterpreted as text")
    if kv.attribute.isInstanceOf[Attribute.Nested]
    then
      container.copy(content =
        Text(InlineText(s"${kv.indent}${kv.attribute.id}={") +: kv.attribute.text.inl :+ InlineText("}"))
      )(
        container.prov
      )
    else
      container.copy(content = Text(InlineText(s"${kv.indent}${kv.attribute.id}=") +: kv.attribute.text.inl))(
        container.prov
      )
  }

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
    CommonParsers.spaceLineF.run
    KeyValue(indent, attribute, prov)
  }

  def unquoted: Scip[Text] = BlockParsers.sectionInlines.map(Text.apply)

  case class Whitespace(content: String):
    override def toString: String = s"Whitespace(»${content.replace("\n", "\\n")}«)"

  def whitespace: Scip[Whitespace] =
    (CommonParsers.verticalSpaces and (DirectiveParsers.commentContent.attempt or eol)).str.map(Whitespace.apply)

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

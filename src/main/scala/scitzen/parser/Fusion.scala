package scitzen.parser

import de.rmgk.scip.{Scip, Scx, all, any, choice, scx, seq, until}
import scitzen.compat.Logging
import scitzen.parser.Atoms.{Atom, Container, Delimited, ListAtom, SectionAtom, Whitespace, annotatedAtom}
import scitzen.parser.CommonParsers.{eol, newline, untilI, untilIS}
import scitzen.parser.{AttributesParser, CommonParsers, DelimitedBlockParsers, DirectiveParsers}
import scitzen.project.{Document, Project}
import scitzen.sast.{
  Attribute, Attributes, BCommand, Block, Directive, Fenced, Inline, InlineText, ListItem, Paragraph, Prov, Sast,
  Section, Slist, SpaceComment, Text
}

import java.nio.file.Path
import scala.annotation.tailrec
import scala.reflect.TypeTest

object Fusion {

  def run(project: Project, absolute: List[Path]): Unit =
    absolute.map: abs =>
      val pp  = project.asProjectPath(abs)
      val doc = Document(pp)
      val res = {
        val content = doc.content

        def newscx() = Scx(
          input = content,
          index = 0,
          maxpos = content.length,
          depth = 0,
          lastFail = -1,
          tracing = false
        )

        parser.runInContext(newscx())
      }
      res.foreach(el => println(el))
    ()

  def parser: Scip[List[Sast]] = Scip {
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

  @tailrec
  def fuseTop(atoms: Atoms, sastAcc: List[Sast]): List[Sast] = {

    atoms match
      case LazyList() => sastAcc.reverse
      case container #:: tail =>
        container.content match
          case dir: Directive => fuseTop(tail, dir :: sastAcc)
          case Atoms.Fenced(commands, attributes, content) =>
            val block = Block(commands, attributes, Fenced(content))(container.prov)
            fuseTop(tail, block :: sastAcc)
          case del: Delimited =>
            val (delimited, rest) = fuseDelimited(container.indent, del, container.prov, tail)
            fuseTop(rest, delimited :: sastAcc)
          case sec: Section =>
            fuseTop(tail, sec :: sastAcc)
          case ListAtom(_, _) =>
            val (list, rest) = fuseList(atoms, Nil)
            fuseTop(rest, list :: sastAcc)
          case _: Whitespace =>
            val (ws, rest) = collectType[Whitespace](atoms)
            val content = ws.map(w => {
              if !w.indent.isBlank then println(s"»${w.indent}«");
              s"${w.indent}${w.content.content}"
            }).mkString
            fuseTop(
              rest,
              Block(
                BCommand.Empty,
                Attributes.empty,
                SpaceComment(content)
              )(combineProvidence(ws)) :: sastAcc
            )
          case _: Text =>
            val (containers, rest) = collectType[Text | Directive](atoms)
            val text = Text(containers.iterator.flatMap: container =>
              val inlineIndent = if container.indent.nonEmpty then List(InlineText(container.indent)) else Nil
              val inlines = container.content match
                case text: Text           => text.inl
                case directive: Directive => List(directive)
              InlineText("\n") +: (inlineIndent concat inlines)
            .drop(1).toSeq).fuse
            fuseTop(
              rest,
              Block(BCommand.Empty, Attributes.empty, Paragraph(text))(combineProvidence(containers)) :: sastAcc
            )
  }

  @tailrec
  def fuseList(atoms: Atoms, acc: List[ParsedListItem]): (Slist, Atoms) = {
    atoms match
      case (cont @ Container(indent, ListAtom(pfx, content))) #:: tail
          if Text(content).plainString.stripTrailing().endsWith(":") =>
        val nextIndent = tail.head.indent
        val (inner, rest) = tail.collectWhile: cont =>
          if cont.indent.startsWith(nextIndent) then
            Some(cont.copy(indent = cont.indent.stripPrefix(nextIndent))(cont.prov))
          else None
        val innerFused = fuseTop(inner, Nil)
        val block      = Block(BCommand.Empty, Attributes.empty, scitzen.sast.Parsed(nextIndent, innerFused))(Prov())
        fuseList(rest, ParsedListItem(s"$indent$pfx", Text(content), cont.prov, Some(block)) :: acc)

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
                  case Text(inl)      => inl
                  case dir: Directive => List(dir)
              )
            )
          }
        fuseList(rest, ParsedListItem(s"$indent$pfx", Text(content concat snippets), cont.prov, None) :: acc)
      case other =>
        (ListConverter.listtoSast(acc.reverse), atoms)
  }

  case class ParsedListItem(marker: String, itemText: Text, prov: Prov, content: Option[Block])

  object ListConverter {

    private def splitted[ID, Item](items: List[(ID, Item)]): Seq[(Item, Seq[Item])] =
      items match {
        case Nil => Nil
        case (marker, item) :: tail =>
          val (take, drop) = tail.span { case (c, _) => marker != c }
          (item -> take.map(_._2)) +: splitted(drop)
      }

    def listtoSast(items: Seq[ParsedListItem]): Slist = {
      /* defines which characters are distinguishing list levels */
      def norm(m: String) = m.replaceAll("""[^\s\*\.•\-]""", "")

      val split = splitted(items.iterator.map(i => (norm(i.marker), i)).toList)

      if (split.isEmpty) Slist(Nil)
      else otherList(split)
    }

    private def otherList(split: Seq[(ParsedListItem, Seq[ParsedListItem])]): Slist = {
      val listItems = split.map {
        case (item, children) =>
          val contentSast = item.content
          val childSasts = if (children.isEmpty) None
          else Some(listtoSast(children))
          ListItem(item.marker, item.itemText, contentSast.orElse(childSasts))
      }
      scitzen.sast.Slist(listItems)
    }
  }

  def fuseDelimited(indent: String, del: Delimited, prov: Prov, atoms: LazyList[Container[Atom]]) = {
    val (innerAtoms, rest) = atoms.span:
      case Container(`indent`, Delimited(del.delimiter, BCommand.Empty, Attributes.empty)) =>
        false
      case other => true
    val adaptedIndent = innerAtoms.map:
      case cont @ Container(cindent, content) =>
        Container(cindent.stripPrefix(indent).stripPrefix("\t"), content)(cont.prov)
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

}

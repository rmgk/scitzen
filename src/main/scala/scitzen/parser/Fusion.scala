package scitzen.parser

import de.rmgk.scip.{Scip, Scx, all, any, choice, scx, seq, until}
import scitzen.compat.Logging
import scitzen.parser.Atoms.{Atom, Container, DefinitionListAtom, Delimited, ListAtom, annotatedAtom}
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
    absolute.foreach: abs =>
      val pp      = project.asProjectPath(abs)
      val doc     = Document(pp)
      val content = doc.content

      parser.runInContext:
        Scx(
          input = content,
          index = 0,
          maxpos = content.length,
          depth = 0,
          lastFail = -1,
          tracing = false
        )
      .foreach(el => println(el))

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
          case unchanged: (Directive | Block | Section | SpaceComment) => fuseTop(tail, unchanged :: sastAcc)
          case del: Delimited =>
            val (delimited, rest) = fuseDelimited(container.indent, del, container.prov, tail)
            fuseTop(rest, delimited :: sastAcc)
          case ListAtom(_, _) =>
            val (list, rest) = fuseList(atoms, Nil)
            fuseTop(rest, list :: sastAcc)
          case DefinitionListAtom(_, _) =>
            val (list, rest) = fuseDefinitionList(atoms, Nil)
            fuseTop(rest, list :: sastAcc)
          case _: Text =>
            val (inner, rest) = collectType[Text | Directive](atoms)
            fuseTop(
              rest,
              Paragraph(inner) :: sastAcc
            )
  }

  private def extractTextRun(atoms: Atoms): (LazyList[Container[Text | Directive]], LazyList[Container[Atom]], Text) = {
    val (containers, rest) = collectType[Text | Directive](atoms)
    val text = Text(containers.iterator.flatMap: container =>
      val inlines = container.content match
        case text: Text           => text.inl
        case directive: Directive => List(directive, InlineText("\n"))
      if container.indent.isEmpty
      then inlines
      else InlineText(container.indent) +: inlines
    .toSeq).fuse
    (containers, rest, text)
  }

  @tailrec
  def fuseList(atoms: Atoms, acc: List[ListItem]): (Slist, Atoms) = {
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
                  case Text(inl)      => inl
                  case dir: Directive => List(dir)
              )
            )
          }
        fuseList(rest, ListItem(s"$indent$pfx", Text(content concat snippets), None) :: acc)
      case other =>
        (ListConverter.listtoSast(acc.reverse), atoms)
  }

  @tailrec
  def fuseDefinitionList(atoms: Atoms, acc: List[ListItem]): (Slist, Atoms) = {
    atoms match
      case (cont @ Container(indent, DefinitionListAtom(pfx, content))) #:: tail =>
        val nextIndent = tail.head.indent
        val (inner, rest) = tail.collectWhile: cont =>
          if cont.indent.startsWith(nextIndent) then
            Some(cont.copy(indent = cont.indent.stripPrefix(nextIndent)))
          else None
        val innerFused = fuseTop(inner, Nil)
        val block      = Block(BCommand.Empty, Attributes.empty, scitzen.sast.Parsed(nextIndent, innerFused))(Prov())
        fuseDefinitionList(rest, ListItem(s"$indent$pfx", Text(content), Some(block)) :: acc)

      case other =>
        (Slist(acc.reverse), atoms)
  }

  object ListConverter {

    private def splitted[ID, Item](items: List[(ID, Item)]): Seq[(Item, Seq[Item])] =
      items match {
        case Nil => Nil
        case (marker, item) :: tail =>
          val (take, drop) = tail.span { (c, _) => marker != c }
          (item -> take.map(_._2)) +: splitted(drop)
      }

    def listtoSast(items: Seq[ListItem]): Slist = {
      /* defines which characters are distinguishing list levels */
      def norm(m: String) = m.replaceAll("""[^\s\*\.•\-]""", "")

      val split = splitted(items.iterator.map(i => (norm(i.marker), i)).toList)

      if (split.isEmpty) Slist(Nil)
      else otherList(split)
    }

    private def otherList(split: Seq[(ListItem, Seq[ListItem])]): Slist = {
      val listItems = split.map {
        case (item, children) =>
          val contentSast = item.content
          val childSasts = if (children.isEmpty) None
          else Some(listtoSast(children))
          ListItem(item.marker, item.text, contentSast.orElse(childSasts))
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
        Container(cindent.stripPrefix(indent).stripPrefix("\t"), content, cont.prov)
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

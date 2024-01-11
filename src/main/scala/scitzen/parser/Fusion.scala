package scitzen.parser

import de.rmgk.scip.{Scip, Scx, all, any, choice, scx, seq, until}
import scitzen.compat.Logging
import scitzen.parser.Atoms.{Atom, Container, DefinitionListAtom, Delimiter, ListAtom, annotatedAtom}
import scitzen.parser.CommonParsers.{eol, newline, untilI, untilIS}
import scitzen.parser.{AttributesParser, CommonParsers, DelimitedBlockParsers, DirectiveParsers}
import scitzen.project.{Document, Project}
import scitzen.sast.{
  Attribute, Attributes, BCommand, DefinitionItem, Directive, Fenced, Inline, InlineText, ListItem, Paragraph, Parsed,
  Prov, Sast, Sdefinition, Section, Slist, SpaceComment, Text
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
          case unchanged: (Directive | Section | SpaceComment | Fenced) => fuseTop(tail, unchanged :: sastAcc)

          case del: Delimiter =>
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

  @tailrec
  def fuseList(atoms: Atoms, acc: List[ListItem]): (Slist, Atoms) = {
    atoms match
      case (cont @ Container(indent, ListAtom(pfx, content))) #:: tail =>
        val (textSnippets, rest) = collectType[Text | Directive](tail)
        fuseList(
          rest,
          ListItem(pfx, indent, Paragraph(Container("", Text(content), cont.prov) +: textSnippets)) :: acc
        )
      case other =>
        (Slist(acc.reverse), atoms)
  }

  @tailrec
  def fuseDefinitionList(atoms: Atoms, acc: List[DefinitionItem]): (Sdefinition, Atoms) = {
    atoms match
      case (cont @ Container(indent, DefinitionListAtom(pfx, content))) #:: tail =>
        val nextIndent = tail.head.indent
        val (inner, rest) = tail.collectWhile: cont =>
          if cont.indent.startsWith(nextIndent)
          then Some(cont)
          else None
        val innerFused = fuseTop(inner, Nil)
        fuseDefinitionList(rest, DefinitionItem(s"$pfx", indent, Text(content), innerFused) :: acc)

      case other =>
        (Sdefinition(acc.reverse), atoms)
  }

  def fuseDelimited(indent: String, del: Delimiter, prov: Prov, atoms: LazyList[Container[Atom]]) = {
    val (innerAtoms, rest) = atoms.span:
      case Container(`indent`, Delimiter(del.`marker`, BCommand.Empty, Attributes.empty)) =>
        false
      case other => true
    val adaptedIndent = innerAtoms.map:
      case cont @ Container(cindent, content) =>
        Container(cindent.stripPrefix(indent).stripPrefix("\t"), content, cont.prov)
    val innerSast = fuseTop(adaptedIndent, Nil)
    (
      scitzen.sast.Parsed(Container(indent, del, prov), innerSast),
      rest.drop(1)
    )

  }

}

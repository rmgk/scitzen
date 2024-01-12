package scitzen.sast

import de.rmgk.scip.{Scip, Scx, all, any, choice, scx, seq, until}
import scitzen.compat.Logging
import scitzen.parser.CommonParsers.{eol, newline, untilI, untilIS}
import scitzen.parser.{AtomParsers, AttributesParser, CommonParsers, DelimitedBlockParsers, DirectiveParsers}
import scitzen.project.{Document, Project}
import scitzen.sast.{
  Attribute, Attributes, BCommand, Directive, Fenced, FusedDefinitionItem, FusedDefinitions, FusedList, FusedListItem,
  Inline, InlineText, Paragraph, Parsed, Prov, Sast, Section, SpaceComment, Text
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

  def atoms: Scip[Atoms] = Scip {
    @tailrec
    def rec(acc: Atoms): Atoms = {
      if scx.index >= scx.maxpos
      then acc.reverse
      else
        val atom = AtomParsers.alternatives.runInContext(scx)
        rec(atom :: acc)
    }
    rec(Nil)
  }

  def parser: Scip[List[Sast]] = Scip {
    fuseTop(atoms.run, Nil)
  }

  extension [A](list: List[A])
    def collectWhile[R](f: A => Option[R]): (List[R], List[A]) =
      list match
        case Nil => (List.empty, list)
        case head :: tail =>
          f(head) match
            case None => (List.empty, list)
            case Some(res) =>
              val (good, bad) = tail.collectWhile(f)
              (res :: good, bad)

  def collectType[As <: Atom](atoms: Atoms)(using
      TypeTest[Atom, As]
  ): (List[Container[As]], List[Container[Atom]]) =
    atoms.collectWhile:
      case cont @ Container(_, ws: As) => Some(cont.asInstanceOf[Container[As]])
      case other                       => None

  type Atoms = List[Container[Atom]]

  @tailrec
  def fuseTop(atoms: Atoms, sastAcc: List[Sast]): List[Sast] = {

    atoms match
      case List() => sastAcc.reverse
      case container :: tail =>
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
  def fuseList(atoms: Atoms, acc: List[FusedListItem]): (FusedList, Atoms) = {
    atoms match
      case (cont @ Container(indent, la: ListAtom)) :: tail =>
        val (textSnippets, rest) = collectType[Text | Directive](tail)
        fuseList(
          rest,
          FusedListItem(cont.copy(content = la), textSnippets) :: acc
        )
      case other =>
        (FusedList(acc.reverse), atoms)
  }

  @tailrec
  def fuseDefinitionList(atoms: Atoms, acc: List[FusedDefinitionItem]): (FusedDefinitions, Atoms) = {
    atoms match
      case (cont @ Container(indent, dla: DefinitionListAtom)) :: tail =>
        val nextIndent = tail.head.indent
        val (inner, rest) = tail.collectWhile: cont =>
          if cont.indent.startsWith(nextIndent)
          then Some(cont)
          else None
        val innerFused = fuseTop(inner, Nil)
        fuseDefinitionList(rest, FusedDefinitionItem(cont.copy(content = dla), innerFused) :: acc)

      case other =>
        (FusedDefinitions(acc.reverse), atoms)
  }

  def fuseDelimited(indent: String, del: Delimiter, prov: Prov, atoms: Atoms) = {
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

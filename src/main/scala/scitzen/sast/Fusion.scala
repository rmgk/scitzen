package scitzen.sast

import de.rmgk.scip.{Scip, Scx, all, any, choice, scx, seq, until}
import scitzen.compat.Logging
import scitzen.parser.CommonParsers.{eol, newline, untilI, untilIS}
import scitzen.parser.{AtomParsers, AttributesParser, CommonParsers, DelimitedBlockParsers, DirectiveParsers}
import scitzen.project.{Document, Project}
import scitzen.sast.{
  Attribute, Attributes, BCommand, Directive, Fenced, FusedDefinitionItem, FusedDefinitions, FusedList, FusedListItem,
  Inline, InlineText, Paragraph, FusedDelimited, Prov, Sast, Section, SpaceComment
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
  ): (List[As], List[Atom]) =
    atoms.collectWhile:
      case ws: As => Some(ws)
      case other  => None

  type Atoms = List[Atom]

  @tailrec
  def fuseTop(atoms: Atoms, sastAcc: List[Sast]): List[Sast] = {

    atoms match
      case List() => sastAcc.reverse
      case container :: tail =>
        container match
          case unchanged: (Directive | Section | SpaceComment | Fenced) => fuseTop(tail, unchanged :: sastAcc)

          case del: Delimiter =>
            val (delimited, rest) = fuseDelimited(container.meta.indent, del, tail)
            fuseTop(rest, delimited :: sastAcc)
          case ListAtom(_, _, _) =>
            val (list, rest) = fuseList(atoms, Nil)
            fuseTop(rest, list :: sastAcc)
          case DefinitionListAtom(_, _, _) =>
            val (list, rest) = fuseDefinitionList(atoms, Nil)
            fuseTop(rest, list :: sastAcc)
          case _: TextAtom =>
            val (inner, rest) = collectType[TextAtom | Directive](atoms)
            fuseTop(
              rest,
              Paragraph(inner) :: sastAcc
            )
  }

  @tailrec
  def fuseList(atoms: Atoms, acc: List[FusedListItem]): (FusedList, Atoms) = {
    atoms match
      case (la: ListAtom) :: tail =>
        val (textSnippets, rest) = collectType[TextAtom | Directive](tail)
        fuseList(
          rest,
          FusedListItem(la, textSnippets) :: acc
        )
      case other =>
        (FusedList(acc.reverse), atoms)
  }

  @tailrec
  def fuseDefinitionList(atoms: Atoms, acc: List[FusedDefinitionItem]): (FusedDefinitions, Atoms) = {
    atoms match
      case (dla: DefinitionListAtom) :: tail =>
        val nextIndent = tail.head.meta.indent
        val (inner, rest) = tail.collectWhile: cont =>
          if cont.meta.indent.startsWith(nextIndent)
          then Some(cont)
          else None
        val innerFused = fuseTop(inner, Nil)
        fuseDefinitionList(rest, FusedDefinitionItem(dla, innerFused) :: acc)

      case other =>
        (FusedDefinitions(acc.reverse), atoms)
  }

  def fuseDelimited(indent: String, del: Delimiter, atoms: Atoms) = {
    val (innerAtoms, rest) = atoms.span:
      case Delimiter(del.`marker`, BCommand.Empty, Attributes.empty, Meta(`indent`)) =>
        false
      case other => true
    val innerSast = fuseTop(innerAtoms, Nil)
    (
      scitzen.sast.FusedDelimited(del, innerSast),
      rest.drop(1)
    )

  }

}

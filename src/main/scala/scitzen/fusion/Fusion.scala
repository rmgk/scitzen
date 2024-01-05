package scitzen.fusion

import de.rmgk.scip.{Scip, Scx}
import scitzen.parser.ListParsers.ParsedListItem
import scitzen.parser.{BlockParsers, CommonParsers, DirectiveParsers, ListParsers}
import scitzen.project.{Document, Project}
import scitzen.sast.{Block, Directive, Sast, Section}

import java.nio.file.Path

object Fusion {

  type Atom = Directive | ParsedListItem | Block | Section

  def run(project: Project, absolute: List[Path]): Unit =
    absolute.map: abs =>
      val pp = project.asProjectPath(abs)
      val doc  = Document(pp)
      val res = documentUnwrap(doc)
      res.foreach(el => println(el))
    ()

  def documentUnwrap(doc: Document): List[Atom] = {
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

    List.unfold(scx): scx =>
      if scx.index >= scx.maxpos
      then None
      else Some(alternatives.runInContext(scx), scx)
  }

  def alternatives: Scip[Atom] =
    (BlockParsers.extendedWhitespace.trace("whitespace") |
      ListParsers.simpleListItem.trace("simple list item") |
      //DelimitedBlockParsers.anyDelimited.trace("block delim") |
      BlockParsers.sectionTitle.trace("block title") |
      (DirectiveParsers.full <~ CommonParsers.spaceLineF).trace("block directive") |
      BlockParsers.paragraph.trace("block para")).trace("block")
}

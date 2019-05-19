package scitzen.outputs

import scitzen.generic.Sast
import scitzen.generic.Sast.{TLBlock, ParsedBlock, Section}
import scalatags.Text.all._

object HtmlToc {

  def tableOfContents(document: Seq[TLBlock], tocDepth: Int): Option[Frag] = {
    tableOfContentsS(document.map(_.content), tocDepth)
  }


    def tableOfContentsS(document: Seq[Sast], tocDepth: Int): Option[Frag] = {
    def findSections(cont: Seq[Sast]): Seq[Section] = {
      cont.flatMap {
        case s: Section              => List(s)
        case tlb: TLBlock     => findSections(List(tlb.content))
        case ParsedBlock(_, content) => findSections(content.map(_.content))
        case _                       => Nil
      }
    }

    def makeToc(cont: Seq[Sast], depth: Int): Option[Tag] = {
      findSections(cont) match {
        case Nil      => None
        case sections =>
          Some(ol(sections.map {
            case Section(title, inner) =>
              val sub = if (depth > 1) makeToc(inner.map(_.content), depth - 1) else None
              li(a(href := s"#${title.str}", title.str))(sub)
          }))
      }
    }


    document match {
      case Seq(Section(_, secCon)) => makeToc(secCon.map(_.content), tocDepth)
      case other                   => makeToc(other, tocDepth)
    }
  }
}
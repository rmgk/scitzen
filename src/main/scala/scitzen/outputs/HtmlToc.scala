package scitzen.outputs

import scitzen.generic.Sast
import scitzen.generic.Sast.{AttributedBlock, ParsedBlock, Section}
import scalatags.Text.all._

object HtmlToc {
  def tableOfContents(document: Seq[Sast], tocDepth: Int): Option[Frag] = {
    def findSections(cont: Seq[Sast]): Seq[Section] = {
      cont.flatMap {
        case s: Section => List(s)
        case AttributedBlock(_, content) => findSections(List(content))
        case ParsedBlock(_, content) => findSections(content)
        case _ => Nil
      }
    }

    def makeToc(cont: Seq[Sast], depth: Int): Option[Tag] = {
      findSections(cont) match {
        case Nil      => None
        case sections =>
          Some(ol(sections.map {
            case Section(title, inner) =>
              val sub = if (depth > 1) makeToc(inner, depth - 1) else None
              li(a(href := s"#${title.str}", title.str))(sub)
          }))
      }
    }


    document match {
      case Seq(Section(_, secCon)) => makeToc(secCon, tocDepth)
      case other                   => makeToc(other, tocDepth)
    }
  }
}
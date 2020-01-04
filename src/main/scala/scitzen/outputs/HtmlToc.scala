package scitzen.outputs

import scalatags.Text.all._
import scitzen.generic.Sast
import scitzen.generic.Sast.{Parsed, SBlock, Section}

object HtmlToc {

  def tableOfContents(document: Seq[Sast], tocDepth: Int): Option[Frag] = {
    tableOfContentsS(document, tocDepth)
  }


  def tableOfContentsS(document: Seq[Sast], tocDepth: Int): Option[Frag] = {
    def findSections(cont: Seq[Sast]): Seq[Section] = {
      cont.flatMap {
        case s: Section                    => List(s)
        case SBlock(_, Parsed(_, content)) => findSections(content)
        case _                             => Nil
      }
    }

    def makeToc(cont: Seq[Sast], depth: Int): Option[Tag] = {
      findSections(cont) match {
        case sections if sections.length > 1 =>
          Some(ol(sections.map {
            case Section(title, inner, attr) =>
              val label = attr.named.getOrElse("label", title.str)
              val sub   = if (depth > 1) makeToc(inner, depth - 1) else None
              li(a(href := s"#$label", title.str))(sub)
          }))

        case _ => None
      }
    }


    document match {
      case Seq(Section(_, secCon, _)) => makeToc(secCon, tocDepth)
      case other                      => makeToc(other, tocDepth)
    }
  }
}

package scitzen.outputs

import scalatags.Text.all._
import scitzen.parser.Sast.{Parsed, Block, Section}
import scitzen.parser.Sast

object HtmlToc {

  def tableOfContents(document: Seq[Sast]): Option[Frag] = {
    def makeToc(cont: Seq[Sast]): Option[Tag] = {
      val allSections          = getSections(cont).filterNot(_.prefix == "=")
      val firstStructuralLevel = allSections.filter(_.prefix.contains('='))
      val sections =
        if (firstStructuralLevel.sizeIs > 1) firstStructuralLevel
        else allSections
      if (sections.length <= 1) None
      else {
        Some(ol(sections.map {
          case Section(title, level, attr) =>
            val label = attr.named.getOrElse("label", title.str)
            li(a(href := s"#$label", title.str))
        }))
      }
    }

    document match {
      case other => makeToc(other)
    }
  }

  private def getSections(cont: Seq[Sast]): Seq[Section] = {
    cont.collect {
      case s: Section                   => List(s)
      case Block(_, Parsed(_, content)) => getSections(content)
    }.flatten
  }
}

package scitzen.outputs

import scalatags.Text.all._
import scitzen.parser.Sast.Section

object HtmlToc {
  def tableOfContents(docsections: Seq[Section]): Option[Frag] = {
    val allSections          = docsections.filterNot(_.prefix == "=")
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
}

package scitzen.outputs

import scalatags.Text.all._
import scitzen.generic.Sast
import scitzen.generic.Sast.{Parsed, SBlock, Section}

object HtmlToc {

  def tableOfContents(document: Seq[Sast], tocDepth: Int): Option[Frag] = {
    tableOfContentsS(document, tocDepth)
  }


  def tableOfContentsS(document: Seq[Sast], tocDepth: Int): Option[Frag] = {

    //if (tocDepth != 2) scribe.warn(s"toc with more then one level of depth currently unsuported")

    def makeToc(cont: Seq[Sast], depth: Int): Option[Tag] = {
      val sections = getSections(cont)
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
      case other => makeToc(other, tocDepth)
    }
  }

  private def getSections(cont: Seq[Sast]): Seq[Section] = {
    cont.collect {
      case s: Section => List(s)
      case SBlock(_, Parsed(_, content)) => getSections(content)
    }.flatten
  }
}

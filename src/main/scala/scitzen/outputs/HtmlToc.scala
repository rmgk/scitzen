package scitzen.outputs

import scalatags.Text.all._
import scitzen.parser.Sast.Section

object HtmlToc {

  val maxdepth = 2

  def tableOfContents(docsections: List[Section]): Option[Frag] = {

    def recurse(remaining: List[Section], depth: Int): Option[Frag] = {
      remaining match {
        case Nil => None
        case (head @ Section(title, level, attr)) :: rest =>
          val (sub, other) = rest.span(e => e > head)
          val subtags       = if (depth < maxdepth) recurse(sub, depth + 1) else None
          val label        = attr.named.getOrElse("label", title.str)
          val thistag = li(a(href := s"#$label", title.str), subtags.map(ol(_)))
          val nexttag = recurse(other, depth)
          Some(SeqFrag(thistag :: nexttag.toList))
      }
    }

    recurse(docsections, 1).map(ol(_))
  }
}


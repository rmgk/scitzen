package scitzen.outputs

import scalatags.Text.all._
import scitzen.sast.Section

object HtmlToc:

  val maxdepth = 2

  def tableOfContents(docsections: List[Section]): Option[Frag] =

    def recurse(remaining: List[Section], depth: Int): Option[Frag] =
      remaining match
        case Nil => None
        case (head @ Section(title, _, attr)) :: rest =>
          val (sub, other) = rest.span(e => e > head)
          val subtags      = if depth < maxdepth then recurse(sub, depth + 1) else None
          val thistag      = li(a(href := s"#${head.id}", title.str), subtags.map(ol(_)))
          val nexttag      = recurse(other, depth)
          Some(SeqFrag(thistag :: nexttag.toList))

    recurse(docsections, 1).map(ol(_))

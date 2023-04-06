package scitzen.outputs

import scalatags.Text.all.*
import scitzen.sast.{Attributes, Section, Text, Prov}
import math.Ordering.Implicits.infixOrderingOps

object HtmlToc:

  val maxdepth     = 2
  val startsection = List("==", "#")

  def tableOfContents(docsections: List[Section]): Option[Frag] =

    def recurse(remaining: List[Section], depth: Int): Option[Frag] =
      remaining match
        case Nil => None
        case (head @ Section(title, _, attr)) :: rest =>
          val (sub, other) = rest.span(e => e > head)
          val subtags      = if depth < maxdepth then recurse(sub, depth + 1) else None
          val thistag      = li(a(href := s"#${head.ref}", title.plainString), subtags.map(ol(_)))
          val nexttag      = recurse(other, depth)
          Some(SeqFrag(thistag :: nexttag.toList))

    recurse(docsections.dropWhile(s => !startsection.contains(s.prefix)), 1).map(ol(_))

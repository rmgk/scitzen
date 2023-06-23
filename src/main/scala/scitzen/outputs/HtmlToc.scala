package scitzen.outputs

import scalatags.Text.all.*
import scitzen.contexts.ConversionContext
import scitzen.sast.Section

import math.Ordering.Implicits.infixOrderingOps

object HtmlToc:

  val maxdepth     = 2
  val startsection = List("==", "#")

  def tableOfContents(docsections: List[Section], converter: SastToHtmlConverter): Option[Frag] =

    def recurse(remaining: List[Section], depth: Int): Option[Frag] =
      remaining match
        case Nil => None
        case (head @ Section(title, _, _)) :: rest =>
          val (sub, other) = rest.span(e => e > head)
          val subtags      = if depth < maxdepth then recurse(sub, depth + 1) else None
          val res = converter.convertInlineSeq(ConversionContext(()), title.inl)
          val thistag      = li(a(href := s"#${head.ref}", res.data), subtags.map(ol(_)))
          val nexttag      = recurse(other, depth)
          Some(SeqFrag(thistag :: nexttag.toList))

    recurse(docsections.dropWhile(s => !startsection.contains(s.prefix)), 1).map(ol(_))

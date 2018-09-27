package vitzen.docparser

import asciimedic._
import scalatags.Text.Tag
import scalatags.Text.implicits._
import scalatags.Text.tags.{code, div, p, frag, tag, img}
import scalatags.Text.attrs.{href, src, cls}


object HtmlConverter {
  def convert(document: Document): String = frag(
    document.blocks.filterNot(_.isInstanceOf[asciimedic.WhitespaceBlock]).map(blockToHtml): _*
  ).render

  def blockToHtml(b: Block): Frag = b match {
    case asciimedic.Paragraph(text) => p(
      text.map {
        case InlineText(str) => stringFrag(str)
        case im: InlineMacro => code(im.toString)
        case attr: AttrRef   => code(attr.toString)
      }: _*
    )

    case SectionTitle(level, title) => tag("h" + (level + 1))(title)

    case BlockWithAttributes(block, attributes, title) =>
      frag(
        title.getOrElse("").toString,
        attributes.toString(),
        blockToHtml(block)
      )

    case BlockMacro("image", target, attributes) =>
      div(cls := "imageblock",
          img(src := target)
      )

    case other => div(stringFrag(other.toString))
  }
}

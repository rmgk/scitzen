package vitzen.docparser

import asciimedic.{AttrRef, Block, Document, InlineMacro, InlineText}
import scalatags.Text.Tag
import scalatags.Text.implicits._
import scalatags.Text.tags.{code, div, p, frag, tag}


object HtmlConverter {
  def convert(document: Document): String = frag(document.blocks.map(blockToHtml): _*).render

  def blockToHtml(b: Block): Tag = b match {
    case asciimedic.Paragraph(text) => p(
      text.map{
        case InlineText(str) => stringFrag(str)
        case im: InlineMacro => code(im.toString)
        case attr: AttrRef     => code(attr.toString)
      }: _*
    )
    case asciimedic.SectionTitle(level, title) => tag("h" + (level + 1))(title)
    case other => div(stringFrag(other.toString))
  }
}

package vitzen.docparser

import asciimedic._
import scalatags.Text.implicits._
import scalatags.Text.tags.{code, div, p, frag, tag, img, pre}
import scalatags.Text.attrs.{href, src, cls}


object HtmlConverter {
  def convert(document: Document): String = frag(
    document.blocks.map(blockToHtml(_)): _*
  ).render

  def blockToHtml(b: Block, addModifier: Seq[Modifier] = Nil): Frag = b match {



    case SectionTitle(level, title) => tag("h" + (level + 1))(title)

    case bwa@BlockWithAttributes(block, attributes, title) =>
      frag(
        title.getOrElse("").toString,
        blockToHtml(block, bwa.role.map(c => cls := s" $c "))
      )

    case NormalBlock(BlockType.Whitespace, _) => frag()

    case BlockMacro("image", target, attributes) =>
      div(cls := "imageblock",
          img(src := target)
      )

    case NormalBlock(blockType, text) => {
      blockType match {
        case BlockType.Delimited(delimiter) if delimiter.startsWith(".") => p(text, cls:=" literalblock ")(addModifier : _*)
        case other =>       p(text)

      }
    }

    case other => div(stringFrag(other.toString))
  }
}

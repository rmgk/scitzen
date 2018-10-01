package vitzen.docparser

import asciimedic._
import scalatags.Text.implicits._
import scalatags.Text.tags.{code, div, p, frag, tag, img, pre, blockquote, cite, ul, li}
import scalatags.Text.attrs.{href, src, cls}


object HtmlConverter {
  def convert(document: Document): String = frag(
    document.blocks.map(blockToHtml(_)): _*
  ).render

  def blockToHtml(b: Block, addModifier: Seq[Modifier] = Nil): Frag = b match {



    case SectionTitle(level, title) => tag("h" + (level + 1))(title)

    case bwa : BlockWithAttributes =>
      val positiontype = bwa.positional.headOption
      positiontype match {
        case Some("quote") =>
          val bq = blockquote(blockToHtml(bwa.block))
          val title = bwa.positional.lift(2).fold("")(t => s" $t")
          if (bwa.positional.size > 1) bq(cite(s"– ${bwa.positional(1)}.$title"))
          else bq
        case other =>       frag(
            bwa.title.getOrElse("").toString,
            blockToHtml(bwa.block, bwa.role.map(c => cls := s" $c "))
          )
      }

    case ListBlock(items) =>
      ul(
        items.map(i => li(i.content)): _*
      )

    case NormalBlock(BlockType.Whitespace, _) => frag()

    case BlockMacro("image", target, attributes) =>
      div(cls := "imageblock",
          img(src := target)
      )

    case NormalBlock(blockType, text) => {
      blockType match {
        case BlockType.Delimited(delimiter) if delimiter.startsWith(".") =>
          p(text, cls:=" literalblock ")(addModifier : _*)

        case other =>       p(text)

      }
    }

    case other => div(stringFrag(other.toString))
  }
}
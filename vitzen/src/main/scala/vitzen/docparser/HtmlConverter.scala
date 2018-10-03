package vitzen.docparser

import asciimedic._
import scalatags.Text.implicits._
import scalatags.Text.tags.{code, div, p, frag, tag, img, pre, blockquote, cite, ul, li, strong, em, h6, hr}
import scalatags.Text.tags2.{section}
import scalatags.Text.attrs.{href, src, cls}


object HtmlConverter {
  def convert(document: Document): String = frag(
    document.blocks.map(blockToHtml(_)): _*
  ).render

  def blockToHtml(b: Block, addModifier: Seq[Modifier] = Nil): Frag = b match {



    case SectionTitle(level, title) => tag("h" + (level + 1))(title)

    case bwa: BlockWithAttributes =>
      val positiontype = bwa.positional.headOption
      positiontype match {
        case Some("quote") =>
          val bq = blockquote(blockToHtml(bwa.block))
          val title = bwa.positional.lift(2).fold("")(t => s" $t")
          if (bwa.positional.size > 1) bq(cite(s"– ${bwa.positional(1)}.$title"))
          else bq
        case other         =>
          val blockContent = blockToHtml(bwa.block, bwa.role.map(c => cls := s" $c "))

          bwa.title match {
            case None        => blockContent
            case Some(value) => section(h6(value), blockContent)
          }
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
    case BlockMacro("horizontal-rule", target, attributes) =>
      hr()

    case NormalBlock(blockType, text) => {
      blockType match {
        case BlockType.Delimited(delimiter) if delimiter.startsWith(".") =>
          p(text, cls:=" literalblock ")(addModifier : _*)

        case BlockType.Delimited(delimiter) =>
          pre(delimiter, "\n", text, "\n", delimiter)

        case other =>
          println(s"converting $other block:\n$text")
          p(paragraphStringToHTML(text): _*)

      }
    }

    case other => div(stringFrag(other.toString))
  }

  def paragraphStringToHTML(paragraphString: String) = {
    inlineValuesToHTML(asciimedic.ParagraphParsers.InnerParser().fullParagraph.parse(paragraphString).get.value)
  }

  def inlineValuesToHTML(inners: Seq[Inline]): Seq[Frag] = inners.map[Frag, Seq[Frag]] {
    case InlineText(str) => str
    case InlineQuote(q, inner) => q.head match {
      case '_' => em(inlineValuesToHTML(inner): _*)
      case '*' => strong(inlineValuesToHTML(inner): _*)
    }
    case InlineMacro("//", target, attributes) => frag()
    case InlineMacro(command, target, attributes) =>
      code(s"$command:$target[${attributes.mkString(",")}]")
  }
}

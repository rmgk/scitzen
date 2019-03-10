package scitzen.converter

import scalatags.generic
import scalatags.generic.Bundle
import scitzen.parser._

class HtmlConverter[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT]) {
  import bundle.all._
  import bundle.tags2.aside

  def convert(document: Document): generic.Frag[Builder, FragT] = frag(
    document.blocks.map(blockToHtml): _*
  )


  def splitted[ID, Item](items: Seq[(ID, Item)]): Seq[(Item, Seq[Item])] = items.toList match {
    case Nil                    => Nil
    case (marker, item) :: tail =>
      val (take, drop) = tail.span{ case (c, _) => marker != c }
      (item -> take.map(_._2)) +: splitted(drop)
  }

  def listToHtml(items: Seq[ListItem]): Frag = {
    def norm(m: String) = m.replaceAll("""[^\*\.:-]""", "")
    val split = splitted(items.map(i => (norm(i.marker), i)))

    split match {
      case Nil                 => frag()
      case (firstItem, _) :: _ =>
        val n = norm(firstItem.marker)
        if (n.startsWith(":")) {
          dl(split.flatMap { case (item, contents) =>
            List(
              dt(item.marker),
              dd(paragraphStringToHTML(item.content): _*)(
                item.continuation.toList.flatMap{
                  case NormalBlock(BlockType.Delimited(ws), content) => convertBlockContent(content)
                  case other => List(blockToHtml(other))
                }: _*)(
                listToHtml(contents)
              ))
          }: _*)
        }
        else if (n.startsWith(".")) {
          ol(split.flatMap { case (item, contents) =>
            List(
              li(paragraphStringToHTML(item.content): _*)(
                item.continuation.map(blockToHtml).toList: _*)(
                listToHtml(contents)
              ))
          }: _*)
        } else {
          ul(split.flatMap { case (item, contents) =>
            List(
              li(paragraphStringToHTML(item.content): _*)(
                item.continuation.map(blockToHtml).toList: _*)(
                listToHtml(contents)
              ))
          }: _*)
        }

    }

  }

  def blockToHtml(b: Block): Frag = b match {

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
          val blockContent = blockToHtml(bwa.block) match {
            case any if bwa.role.isEmpty => any
            case tag: Tag => tag(cls := bwa.role.mkString(" "))
            case other => div(other)(cls := bwa.role.mkString(" "))
          }

          bwa.title match {
            case None        => blockContent
            case Some(value) => figure(figcaption(value), blockContent)
          }
      }

    case ListBlock(items) =>
      listToHtml(items)

    case NormalBlock(BlockType.Whitespace, _) => frag()

    case BlockMacro("image", target, attributes) =>
      div(cls := "imageblock",
          img(src := target)
      )
    case BlockMacro("horizontal-rule", target, attributes) =>
      hr()

    case NormalBlock(blockType, text) => {
      blockType match {
        case BlockType.Delimited(delimiter) =>
          if (delimiter.length < 4) div(delimiter, br, text, br, delimiter)
          else delimiter.charAt(0) match {
            // code listing
            case '-' => pre(code(text))
            // literal block
            case '.' => pre(text)
            case '*' => aside(convertBlockContent(text))
            case '_' => blockquote(convertBlockContent(text))
              // there is also '=' example, and '+' passthrough.
              // examples seems rather specific, and passthrough is not implemented.
            case _   => div(delimiter, br, text, br, delimiter)
          }

        case other =>
          p(paragraphStringToHTML(text): _*)

      }
    }

    case other => div(stringFrag(other.toString))
  }

  def paragraphStringToHTML(paragraphString: String): Seq[Frag] = {
    Adoc.paragraph(paragraphString).map(inlineValuesToHTML).get
  }

  def convertBlockContent(blockContent: String): Seq[Frag] = {
    Adoc.document(blockContent).get.blocks.map(blockToHtml)
  }

  def inlineValuesToHTML(inners: Seq[Inline]): Seq[Frag] = inners.map[Frag, Seq[Frag]] {
    case InlineText(str) => str
    case InlineQuote(q, inner) => (q.head match {
      case '_' => em
      case '*' => strong
      case '`' => code
      case '#' => span
    })(inlineValuesToHTML(inner): _*)
    case InlineMacro("//", target, attributes) => frag()
    case InlineMacro(protocol @ ("http" | "https" | "ftp" | "irc" | "mailto"), target, attributes) =>
      val linktarget = s"$protocol:$target"
      linkTo(attributes, linktarget)
    case InlineMacro("link", target, attributes) =>
      linkTo(attributes, target)
    case InlineMacro(command, target, attributes) =>
      code(s"$command:$target[${attributes.mkString(",")}]")
    case AttrRef(id) => code(s"{$id}")
    case other =>
      throw new NotImplementedError(s"does not support $other inline values")
  }
  def linkTo(attributes: Seq[Attribute], linktarget: String) = {
    a(href := linktarget)(attributes.find(_.id == "").fold(linktarget)(_.value))
  }
}

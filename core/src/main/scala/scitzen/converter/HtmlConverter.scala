package scitzen.converter

import scalatags.generic
import scalatags.generic.Bundle
import scitzen.parser._
import cats.implicits._

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
        if (n.startsWith(":")) definitionList(split)
        else otherList(split, n.startsWith("."))
    }
  }

  private def otherList(split: Seq[(ListItem, Seq[ListItem])], ordered: Boolean): Tag = {
    val listItems = split.flatMap { case (item, contents) =>
      List(
        li(paragraphStringToHTML(item.content))(
          item.continuation.map(blockToHtml).toList)(
          listToHtml(contents)
        ))
    }
    (if (ordered) ol else ul)(listItems)
  }
  private def definitionList(split: Seq[(ListItem, Seq[ListItem])]): Tag = {
    dl(split.flatMap { case (item, contents) =>
      try definitionListItem(item, contents)
      catch {
        case e: ParsingAnnotation =>
          println(s"while parsing list ${item.marker}: ${item.content}")
          throw e
      }
    })
  }
  private def definitionListItem(item: ListItem, contents: Seq[ListItem]) = {
    List(
      dt(item.marker),
      dd(paragraphStringToHTML(item.content))(
        continuationToHtml(item))(
        listToHtml(contents)
      ))
  }
  private def continuationToHtml(item: ListItem): SeqNode[Frag] = {
    item.continuation.toList.flatMap {
      case NormalBlock(BlockType.Delimited(_), content) => convertBlockContent(content)
      case other                                         => List(blockToHtml(other))
    }
  }

  def blockToHtml(b: Block): Frag = b match {

    case SectionTitle(level, title) => tag("h" + (level + 1))(title)

    case bwa: BlockWithAttributes =>
      val positiontype = bwa.positional.headOption
      positiontype match {
        case Some("quote") =>
          val innerHtml = bwa.block match {
            case NormalBlock(BlockType.Delimited(_), content) =>
              SeqFrag(convertBlockContent(content))
            case other => blockToHtml(other)
          }
          // for blockquote layout, see example 12 (the twitter quote)
          // http://w3c.github.io/html/textlevel-semantics.html#the-cite-element
          val bq = blockquote(innerHtml)
          // first argument is "quote" we concat the rest and treat them as a single entity
          val title = bwa.positional.drop(1).mkString(", ")
          if (title.nonEmpty) bq(cite(title))
          else bq
        case _         =>
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
      try {
        listToHtml(items)
      }
      catch {
        case e: ParsingAnnotation =>
          println(s"while parsing $items")
          throw e
      }

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
            // Code listing
            // Use this for monospace, space preserving, line preserving text
            // It may wrap to fit the screen content
            case '-' => pre(code(text))
            // Literal block
            // This seems to be supposed to work similar to code? But whats the point then?
            // We interpret this as text with predetermined line wrappings
            // and no special syntax, but otherwise normally formatted.
            // This is great to represent copy&pasted posts or chat messages.
            case '.' => pre(text)
            // Sidebar
            // Parsed as a normal document content, but may float off to some side.
            case '*' => aside(convertBlockContent(text))
            //
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
    Adoc.paragraph(paragraphString).map(inlineValuesToHTML).valueOr(throw _)
  }



  def convertBlockContent(blockContent: String): Seq[Frag] = {
    Adoc.document(blockContent).valueOr(throw _).blocks.map(blockToHtml)
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
    case InlineMacro(tagname@("ins" | "del"), target, attributes) =>
      tag(tagname)(attributes.iterator.filter(_.id.isEmpty).map(_.value).mkString(", "))
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

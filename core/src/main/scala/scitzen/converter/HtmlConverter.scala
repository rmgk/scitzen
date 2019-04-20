package scitzen.converter

import scalatags.generic
import scalatags.generic.Bundle
import scitzen.parser._

class HtmlConverter[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT],
                                                     post: Post
                                                    ) {

  import bundle.all._
  import bundle.tags2.aside

  def convert(): generic.Frag[Builder, FragT] = frag(
    post.document.blocks.map(blockToHtml): _*
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
    item.continuation.toList.map(blockToHtml)
  }

  def blockContentToHtml(b: BlockContent): Frag = {
    b match {

      case SectionTitle(level, title) => if (level <= 1) frag() else tag("h" + level)(id := title, title)

      case ListBlock(items) =>
        try {
          listToHtml(items)
        }
        catch {
          case e: ParsingAnnotation =>
            println(s"while parsing $items")
            throw e
        }

      case WhitespaceBlock(_) | AttributeBlock(_)  => frag()

      case Macro("image", target, attributes) =>
        div(cls := "imageblock",
            img(src := target)
            )

      case NormalBlock(delimiter, text) =>
        if (delimiter == "--") div(delimiter, br, text, br, delimiter)
        else if (delimiter == "") p(paragraphStringToHTML(text): _*)
        else delimiter.charAt(0) match {
          // Code listing
          // Use this for monospace, space preserving, line preserving text
          // It may wrap to fit the screen content
          case '-'| '`'  => pre(code(text))
          // Literal block
          // This seems to be supposed to work similar to code? But whats the point then?
          // We interpret this as text with predetermined line wrappings
          // and no special syntax, but otherwise normally formatted.
          // This is great to represent copy&pasted posts or chat messages.
          case '.' => pre(text)
          // Sidebar
          // Parsed as a normal document content, but may float off to some side.
          case '*' => aside(convertBlockContent(text))
          case '_' => blockquote(convertBlockContent(text))
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case ' ' => SeqFrag(convertBlockContent(text))
          // there is also '=' example, and '+' passthrough.
          // examples seems rather specific, and passthrough is not implemented.
          case _   => div(delimiter, br, text, br, delimiter)
        }

      case other @ Macro(_, _, _) =>
        scribe.warn(s"not implemented: $other")
        div(stringFrag(other.toString))
    }
  }

  def blockToHtml(bwa: Block): Frag = {

    val positiontype = bwa.positional.headOption
    positiontype match {
      case Some("quote") =>
        val innerHtml = bwa.content match {
          case NormalBlock(delimiter, content) => SeqFrag(convertBlockContent(content))
          case other                                           => blockContentToHtml(other)
        }
        // for blockquote layout, see example 12 (the twitter quote)
        // http://w3c.github.io/html/textlevel-semantics.html#the-cite-element
        val bq = blockquote(innerHtml)
        // first argument is "quote" we concat the rest and treat them as a single entity
        val title = bwa.positional.drop(1).mkString(", ")
        if (title.nonEmpty) bq(cite(title))
        else bq
      case _         => blockContentToHtml(bwa.content)
    }


  }


  def convertBlockContent(blockContent: String): Seq[Frag] = {
    Adoc.document(blockContent).right.get.blocks.map(blockToHtml)
  }


  def paragraphStringToHTML(paragraphString: String): Seq[Frag] = {
      Adoc.paragraph(paragraphString).map(inlineValuesToHTML).toTry.get
  }


  def inlineValuesToHTML(inners: Seq[Inline]): Seq[Frag] = inners.map[Frag, Seq[Frag]] {
    case InlineText(str) => str
    case InlineQuote(q, inner) =>
      //scribe.warn(s"inline quote $q: $inner; ${post.sourcePath}")
      (q.head match {
      case '_' => em
      case '*' => strong
      case '`'|'$' => code
    })(inner)
    case Macro("//", target, attributes) => frag()
    case Macro(tagname@("ins" | "del"), target, attributes) =>
      tag(tagname)(attributes.iterator.filter(_.id.isEmpty).map(_.value).mkString(", "))
    case Macro(protocol @ ("http" | "https" | "ftp" | "irc" | "mailto"), target, attributes) =>
      val linktarget = s"$protocol:$target"
      linkTo(attributes, linktarget)
    case Macro("link", target, attributes) =>
      linkTo(attributes, target)
    case im @ Macro(command, target, attributes) =>
      scribe.warn(s"inline macro “$command:$target[$attributes]” for ${post.sourcePath}")
      code(s"$command:$target[${attributes.mkString(",")}]")
  }
  def linkTo(attributes: Seq[Attribute], linktarget: String) = {
    a(href := linktarget)(attributes.find(_.id == "").fold(linktarget)(_.value))
  }
}

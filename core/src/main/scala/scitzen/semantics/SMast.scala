package scitzen.semantics

import scitzen.parser._

sealed trait Sast
case class Sseqf(sasts: Seq[Sast]) extends Sast
object Sseq {
  def apply(sasts: Seq[Sast]): Sseqf = {
    Sseqf(sasts.flatMap {
      case Sseqf(inner) => inner
      case other        => Seq(other)
    })
  }
}
case class Slist(children: Seq[SlistItem]) extends Sast
case class SlistItem(marker: String, content: Sast, inner: Slist)
case class SblockQuote(title: String, content: Sast) extends Sast
case class Sinline(inline: Inline) extends Sast
case class Stitle(level: Int, content: Sast) extends Sast
case class SattributeDef(attribute: Attribute) extends Sast
case class Smacro(imacro: Macro) extends Sast
case class Sblock(delimiter: String, content: Sast) extends Sast
object SastConverter {

  def convert(blocks: Seq[Block]): Sast = Sseq(blocks.map(convertBlock))


  def splitted[ID, Item](items: Seq[(ID, Item)]): Seq[(Item, Seq[Item])] = items.toList match {
    case Nil                    => Nil
    case (marker, item) :: tail =>
      val (take, drop) = tail.span { case (c, _) => marker != c }
      (item -> take.map(_._2)) +: splitted(drop)
  }

  def listToHtml(items: Seq[ListItem]): Slist = {
    def norm(m: String) = m.replaceAll("""[^\*\.:-]""", "")

    val split = splitted(items.map(i => (norm(i.marker), i)))

    split match {
      case Nil                 => Slist(Nil)
      case (firstItem, _) :: _ =>
        val n = norm(firstItem.marker)
        if (n.startsWith(":")) definitionList(split)
        else otherList(split)
    }
  }

  private def otherList(split: Seq[(ListItem, Seq[ListItem])]): Slist = {
    val listItems = split.flatMap { case (item, contents) =>
      List(
        SlistItem(item.marker,
                  Sseq(convertInline(item.content) +:
                       item.continuation.map(convertBlock).toList),
                  listToHtml(contents)
                  ))
    }
    Slist(listItems)
  }
  private def definitionList(split: Seq[(ListItem, Seq[ListItem])]): Slist = {
    Slist(split.map { case (item, contents) => definitionListItem(item, contents) })
  }

  private def definitionListItem(item: ListItem, contents: Seq[ListItem]): SlistItem = {
    SlistItem(item.marker,
              Sseq(convertInline(item.content) +:
                   item.continuation.toList.map(convertBlock)),
              listToHtml(contents)
              )
  }

  def blockContentToHtml(b: BlockContent): Sast = {
    b match {

      case SectionTitle(level, title) => Stitle(level, convertInline(title))

      case ListBlock(items) => listToHtml(items)

      case AttributeBlock(attribute) => SattributeDef(attribute)

      case m: Macro => Smacro(m)

      case WhitespaceBlock(_) => Sseq(Nil)

      case NormalBlock(delimiter, text) =>
        if (delimiter == "") Sblock("", convertInline(text))
        else delimiter.charAt(0) match {
          case '`' | '.' => Sblock(delimiter, Sinline(InlineText(text)))
          case '_' | ' ' => Sblock(delimiter, convertDocument(text))
          case other     =>
            scribe.warn(s"mismatched block $delimiter: $text")
            Sseq(Nil)
        }

    }
  }

  def convertBlock(bwa: Block): Sast = {

    val positiontype = bwa.positional.headOption
    val inner = blockContentToHtml(bwa.content)
    positiontype match {
      case Some("quote") =>
        // first argument is "quote" we concat the rest and treat them as a single entity
        val title = bwa.positional.drop(1).mkString(", ")
        SblockQuote(title, inner)
      case _             => inner
    }


  }


  def convertDocument(blockContent: String): Sast = {
    Sseq(Parse.document(blockContent).right.get.blocks.map(convertBlock))
  }


  def convertInline(paragraphString: String): Sast = {
    Sseq(Parse.paragraph(paragraphString).map(_.map(Sinline.apply)).toTry.get)
  }
}

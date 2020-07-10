package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import Sast.{NoContent, Paragraph, Block, Slist, SlistItem, Text}
import scitzen.parser.CommonParsers._

object ListParsers {

  def simpleMarker[_: P]: P[String] =
    P(verticalSpaces
      ~ ("-" | "•"
        | "*"
        | (digits.? ~ "."))
      ~ verticalSpace).!

  def listContent[_: P]: P[Block] =
    P(withProv(untilE(eol ~ (spaceLine | simpleMarker).map(_ => ()))) ~ eol)
      .map { case (str, prov) =>
        val sast: Seq[Inline] = Parse.inlineUnwrap(str, prov)
        Block(Attributes(Nil, prov), Paragraph(Text(sast)))
      }

  def descriptionListContent[_: P]: P[(String, Prov)] =
    P(withProv(untilE(":" ~ verticalSpaces ~ eol | eol) ~ ":" ~ verticalSpaces ~ eol))

  def simpleListItem[_: P]: P[ListItem] =
    P(simpleMarker ~ listContent).map(ListItem.apply)

  def descriptionListItem[_: P]: P[ListItem] =
    P(simpleMarker ~ descriptionListContent ~
      DelimitedBlockParsers.whitespaceLiteral.?)
      .map {
        case (marker, (str, prov), innerBlock) =>
          val sast: Seq[Inline] = Parse.inlineUnwrap(str, prov)
          ListItem(marker, Block(Attributes(Nil, prov), Paragraph(Text(sast))), innerBlock)
      }

  def list[_: P]: P[Slist] =
    P((descriptionListItem | simpleListItem).rep(1)).map { listItems =>
      ListConverter.listtoSast(listItems)
    }

}


case class ListItem(marker: String, text: Block, content: Option[Block])
case object ListItem {
  def apply(mc: (String, Block)): ListItem = ListItem(mc._1, mc._2, None)
}

object ListConverter {

  def splitted[ID, Item](items: Seq[(ID, Item)]): Seq[(Item, Seq[Item])] =
    items.toList match {
      case Nil => Nil
      case (marker, item) :: tail =>
        val (take, drop) = tail.span { case (c, _) => marker != c }
        (item -> take.map(_._2)) +: splitted(drop)
    }

  def listtoSast(items: Seq[ListItem]): Slist = {
    /* defines which characters are distinguishing list levels */
    def norm(m: String) = m.replaceAll("""[^\s\*\.•\-]""", "")

    val split = splitted(items.map(i => (norm(i.marker), i)))

    if (split.isEmpty) Slist(Nil) else otherList(split)
  }

  private def otherList(split: Seq[(ListItem, Seq[ListItem])]): Slist = {
    val listItems = split.map {
      case (item, children) =>
        val itemSast    = item.text.content.asInstanceOf[Paragraph].content
        val contentSast = item.content
        val childSasts  = if (children.isEmpty) None else Some(listtoSast(children))
        SlistItem(item.marker, itemSast, contentSast.orElse(childSasts).getOrElse(NoContent))
    }
    Slist(listItems)
  }
}

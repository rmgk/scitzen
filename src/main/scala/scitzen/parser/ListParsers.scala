package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.parser.Sast.{Block, ListItem, NoContent, Slist, Text}

object ListParsers {

  def simpleMarker[_: P]: P[String] =
    P(verticalSpaces
      ~ ("-" | "•"
        | "*"
        | (digits.? ~ "."))
      ~ verticalSpace).!

  def simpleListItem[_: P]: P[ParsedListItem] =
    P(simpleMarker ~ withProv(untilE(eol ~ (spaceLine | simpleMarker).map(_ => ()))) ~ eol).map {
      case (marker, (content, prov)) =>
        ParsedListItem(marker, content, prov, None)
    }

  def descriptionListContent[_: P]: P[(String, Prov)] =
    P(withProv(untilE(":" ~ verticalSpaces ~ eol | eol) ~ ":" ~ verticalSpaces ~ eol))
  def descriptionListItem[_: P]: P[ParsedListItem] =
    P(simpleMarker ~ descriptionListContent ~
      DelimitedBlockParsers.whitespaceLiteral.?)
      .map {
        case (marker, (str, prov), innerBlock) =>
          ParsedListItem(marker, str, prov, innerBlock)
      }

  def list[_: P]: P[Slist] =
    P((descriptionListItem | simpleListItem).rep(1)).map { listItems =>
      ListConverter.listtoSast(listItems)
    }

  case class ParsedListItem(marker: String, itemText: String, prov: Prov, content: Option[Block])

  object ListConverter {

    def splitted[ID, Item](items: Seq[(ID, Item)]): Seq[(Item, Seq[Item])] =
      items.toList match {
        case Nil => Nil
        case (marker, item) :: tail =>
          val (take, drop) = tail.span { case (c, _) => marker != c }
          (item -> take.map(_._2)) +: splitted(drop)
      }

    def listtoSast(items: Seq[ParsedListItem]): Slist = {
      /* defines which characters are distinguishing list levels */
      def norm(m: String) = m.replaceAll("""[^\s\*\.•\-]""", "")

      val split = splitted(items.map(i => (norm(i.marker), i)))

      if (split.isEmpty) Slist(Nil) else otherList(split)
    }

    private def otherList(split: Seq[(ParsedListItem, Seq[ParsedListItem])]): Slist = {
      val listItems = split.map {
        case (item, children) =>
          val itemSast    = Parse.inlineUnwrap(item.itemText, item.prov)
          val contentSast = item.content
          val childSasts  = if (children.isEmpty) None else Some(listtoSast(children))
          ListItem(item.marker, Text(itemSast), contentSast.orElse(childSasts).getOrElse(NoContent))
      }
      Slist(listItems)
    }
  }
}

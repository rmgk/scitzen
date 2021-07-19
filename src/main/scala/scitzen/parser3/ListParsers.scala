package scitzen.parser3

import cats.parse.{Numbers, Rfc5234, Parser as P, Parser0 as P0}
import cats.syntax.*
import cats.implicits.*
import cats.parse.Numbers.digits
import cats.parse.Rfc5234.sp
import cats.parse.Parser.*
import CommonParsers._
import scitzen.parser.{DelimitedBlockParsers, Parse}
import scitzen.sast.{Block, ListItem, Prov, Slist, Text}

object ListParsers {

  val simpleMarker: P[String] =
    (verticalSpaces.with1
      ~ ("-" | "•"
        | "*"
        | (digits.?.with1 ~ "."))
      ~ verticalSpace).string

  val simpleListItem: P[ParsedListItem] =
    (simpleMarker ~ withProv(until(eol ~ (spaceLine | simpleMarker).map(_ => ()))) <* eol).map {
      case (marker, (content, prov)) =>
        ParsedListItem(marker, content, prov, None)
    }.withContext("marker")

  val descriptionListContent: P[(String, Prov)] =
    withProv(until(":" ~ verticalSpaces ~ eol | eol) <* ":" <* verticalSpaces <* eol).withContext("content")

  val descriptionListItem: P[ParsedListItem] =
    ((simpleMarker ~ descriptionListContent).backtrack ~
      scitzen.parser3.DelimitedBlockParsers.whitespaceLiteral.?)
      .map {
        case ((marker, (str, prov)), innerBlock) =>
          ParsedListItem(marker, str, prov, innerBlock)
      }.withContext("description")

  val list: P[Slist] =
    (withProv((descriptionListItem | simpleListItem).rep(1))).map {
      case (listItems, _) =>
        ListConverter.listtoSast(listItems.toList)
    }.withContext("list")

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
          val itemSast    = scitzen.parser3.Parse.inlineUnwrap(item.itemText, item.prov)
          val contentSast = item.content
          val childSasts  = if (children.isEmpty) None else Some(listtoSast(children))
          ListItem(item.marker, Text(itemSast), contentSast.orElse(childSasts))
      }
      scitzen.sast.Slist(listItems)
    }
  }
}

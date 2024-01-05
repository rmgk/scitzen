package scitzen.parser
import de.rmgk.scip.*
import scitzen.parser.CommonParsers.*
import scitzen.sast.{Block, ListItem, Prov, Slist, Text}

import java.nio.charset.StandardCharsets

object ListParsers {

  def simpleMarker: Scip[Boolean] =
    (verticalSpaces and ("-•*".any.trace("list symbol") or (digits and ".".all)) and verticalSpace).trace(
      "list marker"
    )

  def simpleListItem: Scip[ParsedListItem] = Scip {
    val marker          = simpleMarker.str.run
    val (content, prov) = withProv(until(eol and (spaceLineB or simpleMarker)).min(0).str <~ eol.orFail).run
    ParsedListItem(marker, content, prov, None)
  }

  def descriptionListContent: Scip[(String, Prov)] =
    withProv(until(((":".all and verticalSpaces and eol) or eol)).min(1).str <~
      (":".all and verticalSpaces and eol).orFail)
  def descriptionListItem: Scip[ParsedListItem] = Scip {
    val marker      = simpleMarker.str.run
    val (str, prov) = descriptionListContent.run
    val innerBlock  = DelimitedBlockParsers.whitespaceLiteral.opt.run
    ParsedListItem(marker, str, prov, innerBlock)
  }

  def list: Scip[Slist] =
    (descriptionListItem | simpleListItem).list(Scip { true }).require(_.nonEmpty).map(ListConverter.listtoSast)

  case class ParsedListItem(marker: String, itemText: String, prov: Prov, content: Option[Block])

  object ListConverter {

    private def splitted[ID, Item](items: List[(ID, Item)]): Seq[(Item, Seq[Item])] =
      items match {
        case Nil => Nil
        case (marker, item) :: tail =>
          val (take, drop) = tail.span { case (c, _) => marker != c }
          (item -> take.map(_._2)) +: splitted(drop)
      }

    def listtoSast(items: Seq[ParsedListItem]): Slist = {
      /* defines which characters are distinguishing list levels */
      def norm(m: String) = m.replaceAll("""[^\s\*\.•\-]""", "")

      val split = splitted(items.iterator.map(i => (norm(i.marker), i)).toList)

      if (split.isEmpty) Slist(Nil) else otherList(split)
    }

    private def otherList(split: Seq[(ParsedListItem, Seq[ParsedListItem])]): Slist = {
      val listItems = split.map {
        case (item, children) =>
          val itemSast    = Parse.inlineUnwrap(item.itemText.getBytes(StandardCharsets.UTF_8))
          val contentSast = item.content
          val childSasts  = if (children.isEmpty) None else Some(listtoSast(children))
          ListItem(item.marker, Text(itemSast), contentSast.orElse(childSasts))
      }
      scitzen.sast.Slist(listItems)
    }
  }
}

package scitzen.scipparse
import scitzen.sast.{Block, ListItem, Prov, Slist, Text}
import de.rmgk.scip.*
import CommonParsers.*
import CompatParsers.*

object ListParsers {

  def simpleMarker: Scip[String] =
    (verticalSpaces
      <~ choice("-".scip, "•".scip, "*".scip, (digits.attempt and ".".scip))
      <~ verticalSpace).str

  def simpleListItem: Scip[ParsedListItem] = Scip {
    val marker          = simpleMarker.run
    val (content, prov) = (withProv(untilE(eol <~ choice(spaceLine, simpleMarker))) <~ eol).run
    ParsedListItem(marker, content, prov, None)
  }

  def descriptionListContent: Scip[(String, Prov)] =
    (withProv(untilE(((":".scip and verticalSpacesB and eolB) or eolB).orFail) <~ ":".scip <~ verticalSpaces <~ eol))
  def descriptionListItem: Scip[ParsedListItem] = Scip {
    val marker      = simpleMarker.run
    val (str, prov) = descriptionListContent.run
    val innerBlock  = DelimitedBlockParsers.whitespaceLiteral.opt.run
    ParsedListItem(marker, str, prov, innerBlock)
  }

  def list: Scip[Slist] =
    choice(descriptionListItem, simpleListItem).list(Scip {true}).require(_.nonEmpty).map(ListConverter.listtoSast)

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
          ListItem(item.marker, Text(itemSast), contentSast.orElse(childSasts))
      }
      scitzen.sast.Slist(listItems)
    }
  }
}

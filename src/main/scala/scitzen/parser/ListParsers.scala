package scitzen.parser
import de.rmgk.scip.*
import scitzen.parser.CommonParsers.*
import scitzen.sast.{Block, ListItem, Prov, Slist, Text}

import java.nio.charset.StandardCharsets

object ListParsers {


  case class ParsedListItem(marker: String, itemText: Text, prov: Prov, content: Option[Block])

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
          val contentSast = item.content
          val childSasts  = if (children.isEmpty) None else Some(listtoSast(children))
          ListItem(item.marker, item.itemText, contentSast.orElse(childSasts))
      }
      scitzen.sast.Slist(listItems)
    }
  }
}

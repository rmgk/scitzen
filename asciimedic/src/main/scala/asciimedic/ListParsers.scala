package asciimedic

import asciimedic.CommonParsers._
import fastparse.all._

object ListParsers {
  val listItemMarker = P("*".rep(1) ~ " ")
  val listContent    = P(untilE(eol ~ (iwsLine | listItemMarker)))
  val listItem       = P((listItemMarker.! ~/ listContent.!)
                         .map((ListItem.apply _).tupled))
  val list           = P(listItem.rep(1, sep = aws ~ Pass)
                         .map(ListBlock) ~ aws)
}

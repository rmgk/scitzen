package asciimedic

import asciimedic.CommonParsers._
import fastparse.all._

/*
 Dose not support attaching to ancestors: https://asciidoctor.org/docs/user-manual/#attaching-to-an-ancestor-list
 */
object ListParsers {

  val normalItemMarker     = P(iws
                               ~ ("-"
                                  | "*".rep(1)
                                  | (digits.? ~ ".".rep(1)))
                               ~ space)
  val definitionItemMarker = P((untilE("::" | eol) ~ ":".rep(2)) ~ (space | newline))
  val itemMarker           = P((normalItemMarker | definitionItemMarker).!)
  val listContent          = P(untilE(eol ~ (iwsLine | itemMarker.map(_ => ()))))
  val listItem             = P((itemMarker.! ~/ listContent.!)
                               .map((ListItem.apply _).tupled))
  val list                 = P(listItem.rep(1, sep = aws ~ Pass)
                               .map(ListBlock) ~ aws)
}

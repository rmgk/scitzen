package asciimedic

import asciimedic.CommonParsers._
import fastparse.all._

/*
 Dose not support attaching to ancestors: https://asciidoctor.org/docs/user-manual/#attaching-to-an-ancestor-list
 */
object ListParsers {

  val innerItemMarker = P("-"
                          | "*".rep(1)
                          | (digits.? ~ ".".rep(1))
                          | (untilE("::" | eol)
                             ~ ":".rep(2)))

  val fullItemMarker = P((iws ~ innerItemMarker).! ~ (space | newline))
  val listContent    = P(untilE(eol ~ (iwsLine | fullItemMarker.map(_ => ()))))
  val listItem       = P((fullItemMarker.! ~/ listContent.!)
                         .map((ListItem.apply _).tupled))
  val list           = P(listItem.rep(1, sep = aws ~ Pass)
                         .map(ListBlock) ~ aws)
}

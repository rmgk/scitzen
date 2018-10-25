package asciimedic

import asciimedic.CommonParsers._
import fastparse._; import fastparse.NoWhitespace._

/*
 Dose not support attaching to ancestors: https://asciidoctor.org/docs/user-manual/#attaching-to-an-ancestor-list
 */
object ListParsers {

  def descriptionItemStart[_:P]: P[String] = P(untilE("::" | eol) ~ ":".rep(2))

  def simpleMarker [_:P]= P(iws
                       ~ ("-"
                          | "*".rep(1)
                          | (digits.? ~ ".".rep(1))
                          | descriptionItemStart)
                       ~ space).!

  def listContent [_:P]= P(untilE(eol ~ (("+".? ~ iwsLine) | (simpleMarker | indentedDescriptionMarker).map(_ => ()))) ~ eol)

  def simpleListItem [_:P]= P((simpleMarker.! ~/ listContent ~ ("+" ~ iwsLine ~ BlockParsers.fullBlock).?)
                         .map((ListItem.apply _).tupled))

  def indentedDescriptionMarker [_:P]= P((descriptionItemStart ~ newline).!).log

  def descriptionListItem[_:P]: P[ListItem] = P(indentedDescriptionMarker ~
                                                ((iwsLine.rep(0) ~
                                                  DelimitedBlockParsers.whitespaceLiteral).map(Right(_)) |
                                                 listContent.map(Left(_))))
                                              .log
                                              .map {
                                                case (m, Right(b)) => ListItem(m, "", Some(b))
                                                case (m, Left(c)) => ListItem(m, c, None)
                                              }


  def list[_:P]: P[ListBlock] = P((simpleListItem | descriptionListItem).rep(1, sep = BlockParsers.extendedWhitespace.?)
                                  ~ iwsLine.?)
                                .map(ListBlock).log


}

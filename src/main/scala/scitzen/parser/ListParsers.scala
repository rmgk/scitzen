package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

/*
 Does not support attaching to ancestors: https://asciidoctor.org/docs/user-manual/#attaching-to-an-ancestor-list
 */
object ListParsers {

  def descriptionMarker[_:P]: P[String] = P(untilE("::" | eol) ~ ":".rep(2)).!

  def simpleMarker [_:P]: P[String] = P(verticalSpaces
                       ~ ("-"
                          | "*".rep(1)
                          | (digits.? ~ ".".rep(1))
                          | descriptionMarker)
                       ~ verticalSpace).!

  def listContent [_:P]: P[(String, Prov)] = P(withProv(untilE(eol ~ (spaceLine | simpleMarker | descriptionMarker).map(_ => ()))) ~ eol)

  def simpleListItem [_:P]: P[ListItem] = P((simpleMarker ~/ listContent.map(NormalBlock("", _)))
                         .map((ListItem.apply _).tupled))


  def descriptionListItem[_:P]: P[ListItem] = P(descriptionMarker ~
                                                ((spaceLine.rep(1) ~
                                                  DelimitedBlockParsers.whitespaceLiteral).map(Right(_)) |
                                                 listContent.map(Left(_))))
                                              .map {
                                                case (m, Right(b)) => ListItem(m, b)
                                                case (m, Left(c)) => ListItem(m, NormalBlock("", c))
                                              }


  def list[_:P]: P[ListBlock] = P((simpleListItem | descriptionListItem).rep(1, sep = "" | spaceLine.rep(1)))
                                .map(ListBlock)


}

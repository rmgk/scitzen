package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

/*
 Dose not support attaching to ancestors: https://asciidoctor.org/docs/user-manual/#attaching-to-an-ancestor-list
 */
object ListParsers {

  def descriptionItemStart[_:P]: P[String] = P(untilE("::" | eol) ~ ":".rep(2))

  def simpleMarker [_:P]: P[String] = P(verticalSpaces
                       ~ ("-"
                          | "*".rep(1)
                          | (digits.? ~ ".".rep(1))
                          | descriptionItemStart)
                       ~ verticalSpace).!

  def listContent [_:P]: P[String] = P(untilE(eol ~ (("+".? ~ spaceLine) | (simpleMarker | indentedDescriptionMarker).map(_ => ()))) ~ eol)

  def simpleListItem [_:P]: P[ListItem] = P((simpleMarker.! ~/ listContent ~ ("+" ~ spaceLine ~ BlockParsers.fullBlock).?)
                         .map((ListItem.apply _).tupled))

  def indentedDescriptionMarker [_:P]: P[String] = P((descriptionItemStart ~ newline).!)

  def descriptionListItem[_:P]: P[ListItem] = P(indentedDescriptionMarker ~
                                                ((spaceLine.rep(0) ~
                                                  DelimitedBlockParsers.whitespaceLiteral).map(Right(_)) |
                                                 listContent.map(Left(_))))
                                              .map {
                                                case (m, Right(b)) => ListItem(m, "", Some(Block(Nil, Prov(), b)))
                                                case (m, Left(c)) => ListItem(m, c, None)
                                              }


  def list[_:P]: P[ListBlock] = P((simpleListItem | descriptionListItem).rep(1, sep = BlockParsers.extendedWhitespace.?)
                                  ~ spaceLine.?)
                                .map(ListBlock)


}

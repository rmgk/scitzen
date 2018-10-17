package asciimedic

import asciimedic.CommonParsers._
import fastparse.all._

/*
 Dose not support attaching to ancestors: https://asciidoctor.org/docs/user-manual/#attaching-to-an-ancestor-list
 */
object ListParsers {

  val descriptionItemStart: Parser[String] = P(untilE("::" | eol) ~ ":".rep(2))

  val simpleMarker = P(iws
                       ~ ("-"
                          | "*".rep(1)
                          | (digits.? ~ ".".rep(1))
                          | descriptionItemStart)
                       ~ space).!

  val listContent = P(untilE(eol ~ (("+".? ~ iwsLine) | (simpleMarker | indentedDescriptionMarker).map(_ => ()))) ~ eol)

  val simpleListItem = P((simpleMarker.! ~/ listContent ~ ("+" ~ iwsLine ~ BlockParsers.fullBlock).?)
                         .map((ListItem.apply _).tupled))

  val indentedDescriptionMarker = P((descriptionItemStart ~ newline).!).log()

  val descriptionListItem: Parser[ListItem] = P(indentedDescriptionMarker ~
                                                ((iwsLine.rep(min = 0) ~
                                                  DelimitedBlockParsers.whitespaceLiteral).map(Right(_)) |
                                                 listContent.map(Left(_))))
                                              .log()
                                              .map {
                                                case (m, Right(b)) => ListItem(m, "", Some(b))
                                                case (m, Left(c)) => ListItem(m, c, None)
                                              }


  val list: Parser[ListBlock] = P((simpleListItem | descriptionListItem).rep(1, sep = BlockParsers.extendedWhitespace.?)
                                  ~ iwsLine.?)
                                .map(ListBlock).log()


}

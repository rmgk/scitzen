package asciimedic

import asciimedic.CommonParsers._
import fastparse.all._

/*
 Dose not support attaching to ancestors: https://asciidoctor.org/docs/user-manual/#attaching-to-an-ancestor-list
 */
object ListParsers {

  val itemMarker  = P(iws
                      ~ ("-"
                         | "*".rep(1)
                         | (digits.? ~ ".".rep(1)))
                      ~ space).!
  val listContent = P(untilE(eol ~ (("+".? ~ iwsLine) | itemMarker.map(_ => ()))) ~ eol)
  val listItem    = P((itemMarker.! ~/ listContent ~ ("+" ~ iwsLine ~ BlockParsers.fullBlock).?)
                      .map((ListItem.apply _).tupled))
  val list: Parser[ListBlock]
                  = P(listItem.rep(1, sep = BlockParsers.extendedWhitespace.?)
                      ~ iwsLine)
                    .map(ListBlock)

  val descriptionItemMarker: Parser[String]               = P(untilE("::" | eol)
                                                              ~ ":".rep(2)
                                                              ~ (space | newline)).!
  val descriptionListItem  : Parser[DescriptionListItem]  = P(descriptionItemMarker
                                                              ~ BlockParsers.extendedWhitespace.?
                                                              ~ BlockParsers.fullBlock)
                                                            .map { case (m, ws, b) => DescriptionListItem(m, b) }.log()
  val descriptionList      : Parser[DescriptionListBlock] = P(descriptionListItem
                                                              .rep(min = 1, sep = BlockParsers.extendedWhitespace.?))
                                                            .map(DescriptionListBlock).log()

}

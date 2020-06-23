package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object ListParsers {

  def simpleMarker[_: P]: P[String] = P(verticalSpaces
                                        ~ ("-" | "•"
                                           | "*".rep(1)
                                           | (digits.? ~ ".".rep(1)))
                                        ~ verticalSpace).!

  def listContent[_: P]: P[(String, Prov)] = P(withProv(untilE(eol ~ (spaceLine | simpleMarker).map(_ => ()))) ~ eol)
  def descriptionListContent[_: P]: P[(String, Prov)] = P(withProv(untilE(":" ~ verticalSpaces ~ eol | eol) ~ ":" ~ verticalSpaces ~ eol))

  def simpleListItem[_: P]: P[ListItem] = P(simpleMarker ~ listContent.map(NormalBlock.apply("", _))).map(ListItem.apply)


  def descriptionListItem[_: P]: P[ListItem] =
    P(simpleMarker ~ descriptionListContent ~
      (significantSpaceLine.rep(0) ~
       DelimitedBlockParsers.whitespaceLiteral).?)
    .map {
      case (m, dls, b) =>
        ListItem(m, NormalBlock("", dls), b)
    }


  def list[_: P]: P[ListBlock] =
    P((descriptionListItem | simpleListItem).rep(1, sep = "" | spaceLine.rep(1))).map(ListBlock)

}

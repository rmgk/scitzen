package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.generic.ListConverter
import scitzen.generic.Sast.{Paragraph, SBlock, Slist, Text}
import scitzen.parser.CommonParsers._

object ListParsers {

  def simpleMarker[_: P]: P[String] =
    P(verticalSpaces
      ~ ("-" | "•"
        | "*".rep(1)
        | (digits.? ~ ".".rep(1)))
      ~ verticalSpace).!

  def listContent[_: P]: P[SBlock] =
    P(withProv(untilE(eol ~ (spaceLine | simpleMarker).map(_ => ()))) ~ eol)
      .map { case (str, prov) =>
        val sast: Seq[Inline] = Parse.valueOrThrow(Parse.paragraph(str, prov))
        SBlock(Attributes(Nil, prov), Paragraph(Text(sast)))
      }

  def descriptionListContent[_: P]: P[(String, Prov)] =
    P(withProv(untilE(":" ~ verticalSpaces ~ eol | eol) ~ ":" ~ verticalSpaces ~ eol))

  def simpleListItem[_: P]: P[ListItem] =
    P(simpleMarker ~ listContent).map(ListItem.apply)

  def descriptionListItem[_: P]: P[ListItem] =
    P(simpleMarker ~ descriptionListContent ~
      DelimitedBlockParsers.whitespaceLiteral.?)
      .map {
        case (marker, (str, prov), innerBlock) =>
          val sast: Seq[Inline] = Parse.valueOrThrow(Parse.paragraph(str, prov))
          ListItem(marker, SBlock(Attributes(Nil, prov), Paragraph(Text(sast))), innerBlock)
      }

  def list[_: P]: P[Slist] =
    P((descriptionListItem | simpleListItem).rep(1)).map { listItems =>
      ListConverter.listtoSast(listItems)
    }

}

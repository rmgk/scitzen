package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._

object HeaderParsers {
  def header[_: P]: P[Header] =
    P(BlockParsers.sectionTitle
      ~ BlockParsers.commentBlock.?
      ~ BlockParsers.extendedWhitespace.?
      ~ AttributeBlockParser.list
      ~ BlockParsers.extendedWhitespace.?)
    .map { case (titlestring, _, _, attr, _) =>
      Header(titlestring, attr.map(_.attribute))
    }
}

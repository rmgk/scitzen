package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object HeaderParsers {
  def title[_: P]: P[String] = P("= " ~/ untilI(eol))
  // asciidoctors revision line is weird https://asciidoctor.org/docs/user-manual/#revision-number-date-and-remark
  // it is clearly not meant for automatic parsing of timestamps and overall … meh
  // authorline is a bit better, but not sure if parsing is worth it.
  def headerline[_: P]: P[String] = P(!":" ~ untilI(eol))
  def header[_: P]: P[Header] =
    P(title
      ~ BlockParsers.commentBlock.?
      ~ headerline.?
      ~ headerline.?
      ~ BlockParsers.extendedWhitespace.?
      ~ AttributeEntry.list
      ~ BlockParsers.extendedWhitespace.?)
    .map { case (titlestring, _, authorline, revline, _, attr, _) =>
      Header(titlestring, authorline.getOrElse(""), revline.getOrElse(""), attr)
    }
}

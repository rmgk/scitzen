package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object HeaderParsers {
  def title     [_:P]: P[String]      = P("= " ~/ untilI(eol))
  def author    [_:P]: P[Author]      = P(untilE(";" | "<" | eol).! ~
                                          quoted(open = "<", close = ">").?)
                                        .map { case (authorName, mail) => Author(authorName, mail) }
  // asciidoctors revision line is weird https://asciidoctor.org/docs/user-manual/#revision-number-date-and-remark
  // it is clearly not meant for automatic parsing of timestamps and overall … meh
  // authorline is a bit better, but not sure if parsing is worth it.
  def revline   [_:P]: P[String]      = P(!":" ~ untilE(eol) ~ eol)
  def authorline[_:P]: P[Seq[Author]] = P(!":" ~ author.rep(sep = anySpaces ~ ";", min = 1) ~ eol)
  def header    [_:P]: P[Header]      =
    P(title
      ~ BlockParsers.commentBlock.?
      ~ authorline.?
      ~ revline.?
      ~ BlockParsers.extendedWhitespace.?
      ~ AttributeEntry.list
      ~ BlockParsers.extendedWhitespace.?)
    .map { case (titlestring, _, al, rl, _, attr, _) =>
      Header(titlestring, al.getOrElse(Nil), attr)
    }
}

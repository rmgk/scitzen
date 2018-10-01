package asciimedic

import asciimedic.CommonParsers._
import fastparse.all._

object HeaderParsers {
  val title     : Parser[String]      = P("= " ~/ untilI(eol))
  val author    : Parser[Author]      = P(untilE(";" | "<" | eol).! ~
                                          quoted(open = Some("<"), close = ">").?)
                                        .map { case (authorName, mail) => Author(authorName, mail) }
  // asciidoctors revision line is weird https://asciidoctor.org/docs/user-manual/#revision-number-date-and-remark
  // it is clearly not meant for automatic parsing of timestamps and overall … meh
  // authorline is a bit better, but not sure if parsing is worth it.
  val revline   : Parser[String]      = P(!":" ~ untilE(eol) ~ eol)
  val authorline: Parser[Seq[Author]] = P(!":" ~ author.rep(sep = aws ~ ";", min = 1) ~ eol)
  val header    : Parser[Header]      = P(title ~ authorline.? ~ revline.? ~ BlockParsers.whitespaceBlock.? ~ Attributes.entry.rep(sep = aws) ~ aws)
                                        .map { case (titlestring, al, ws, rl, attr) =>
                                          Header(titlestring, al.getOrElse(Nil), attr) }
}

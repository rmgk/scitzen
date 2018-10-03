package asciimedic

import asciimedic.CommonParsers._
import fastparse.all._

object Attributes {
  val open  = "["
  val close = "]"

  val reference    : Parser[AttrRef]   = P("{" ~/ identifier.! ~ "}")
                                         .map(AttrRef.apply)
  val equals                           = P(aws ~ "=" ~ aws)
  // https://asciidoctor.org/docs/user-manual/#named-attribute
  // tells us that unquoted attribute values may not contain spaces, however this seems to be untrue in practice
  // however, in the hope of better error messages, we will not allow newlines
  val unquotedValue: Parser[String]    = P(untilE("," | close | eol))
  val value        : Parser[String]    = P(quoted("\"") | quoted("'") | unquotedValue)
  val listDef      : Parser[Attribute] = P(identifier ~ equals ~ value)
                                         .map { case (id, v) => Attribute(id, v) }
  val listValue    : Parser[Attribute] = P(value)
                                         .map(v => Attribute("", v))
  val listElement  : Parser[Attribute] = P(listDef | listValue)
  val xrefAnchorSpecialCase
                   : Parser[Seq[Attribute]]
                                       = P("[" ~ ("[" ~ untilE("]]") ~ "]").! ~ "]")
                                         .map(content => Seq(Attribute("", content)))
  val list         : Parser[Seq[Attribute]]
                                       = P(open ~/ aws ~ listElement.rep(sep = aws ~ "," ~ aws) ~ ",".? ~ aws ~ close)
  val line         : Parser[Seq[Attribute]]
                                       = P((xrefAnchorSpecialCase | list) ~ iwsLine)
}

object AttributeEntry {
  val itemMarker: Parser[String]         = P(":" ~ ("!".? ~ identifier ~ "!".?).! ~ ":")
  val content   : Parser[String]         = P(eol.map(_ => "") | (" " ~ untilI(eol, min = 0).!))
  val entry     : Parser[Attribute]      = P(itemMarker ~/ content)
                                           .map { case (id, v) => Attribute(id, v) }
  val list      : Parser[Seq[Attribute]] = P(entry.rep(sep = BlockParsers.extendedWhitespace.?))
}
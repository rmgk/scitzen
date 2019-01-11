package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object Attributes {
  val  open = "["
  val  close= "]"

  def reference    [_:P]: P[AttrRef]   = P("{" ~/ identifier.! ~ "}")
                                         .map(AttrRef.apply)
  def equals                           [_:P]= P(anySpaces ~ "=" ~ anySpaces)
  // https://asciidoctor.org/docs/user-manual/#named-attribute
  // tells us that unquoted attribute values may not contain spaces, however this seems to be untrue in practice
  // however, in the hope of better error messages, we will not allow newlines
  def unquotedValue[_:P]: P[String]    = P(untilE("," | close | eol))
  def value        [_:P]: P[String]    = P(quoted("\"") | quoted("'") | unquotedValue)
  def listDef      [_:P]: P[Attribute] = P(identifier ~ equals ~ value)
                                         .map { case (id, v) => Attribute(id, v) }
  def listValue    [_:P]: P[Attribute] = P(value)
                                         .map(v => Attribute("", v))
  def listElement  [_:P]: P[Attribute] = P(listDef | listValue)
  def xrefAnchorSpecialCase[_:P]
                   : P[Seq[Attribute]]
                                       = P("[" ~ ("[" ~ untilE("]]") ~ "]").! ~ "]")
                                         .map(content => Seq(Attribute("", content)))
  def list         [_:P]: P[Seq[Attribute]]
                                       = P(open ~/ anySpaces ~ listElement.rep(sep = anySpaces ~ "," ~ anySpaces) ~ ",".? ~ anySpaces ~ close)
  def line         [_:P]: P[Seq[Attribute]]
                                       = P((xrefAnchorSpecialCase | list) ~ spaceLine)
}

object AttributeEntry {
  def itemMarker[_:P]: P[String]         = P(":" ~ ("!".? ~ identifier ~ "!".?).! ~ ":")
  def content   [_:P]: P[String]         = P(eol.map(_ => "") | (" " ~ untilI(eol, min = 0).!))
  def entry     [_:P]: P[Attribute]      = P(itemMarker ~/ content)
                                           .map { case (id, v) => Attribute(id, v) }
  def list      [_:P]: P[Seq[Attribute]] = P(entry.rep(sep = BlockParsers.extendedWhitespace.?))
}
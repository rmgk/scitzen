package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.parser.MacroCommand.Def

object AttributesParser {
  val  open = "["
  val  close= "]"

  def equals       [_:P]: P[Unit]      = P(anySpaces ~ "=" ~ anySpaces)
  // https://asciidoctor.org/docs/user-manual/#named-attribute
  // tells us that unquoted attribute values may not contain spaces, but this seems to be untrue in practice
  // however, in the hope of better error messages, we will not allow newlines
  def unquotedValue[_:P]: P[String]    = P(untilE(";" | close | eol)).map(_.trim)
  def value        [_:P]: P[String]    = P(attrQuotes)
  def listDef      [_:P]: P[Attribute] = P(anySpaces ~ identifier.! ~ equals ~ value)
                                         .map { case (id, v) => Attribute(id, v) }
  def listValue    [_:P]: P[Attribute] = P(value)
                                         .map(v => Attribute("", v))
  def listElement  [_:P]: P[Attribute] = P(listDef | listValue)
  def list         [_:P]: P[Seq[Attribute]] =
    P(open ~ anySpaces ~ listElement.rep(sep = ";") ~ ";".? ~ anySpaces ~ close)
  def line         [_:P]: P[Seq[Attribute]] = P(list ~ spaceLine)

  def attrQuotes[_: P]: P[String] = {
    P(anySpaces ~ "\"".rep.!.flatMap { quotes =>
      (("[" ~ untilI("]" ~ quotes))
       | (if (quotes.isEmpty) unquotedValue
          else untilI(quotes)))
    })
  }

}

object AttributeBlockParser {
  def itemMarker[_: P]: P[String] = P(":" ~ ("!".? ~ identifier.! ~ "!".?).! ~ ":")
  def content[_: P]: P[String] = P((spaceLine ~ DelimitedBlockParsers.whitespaceLiteral).map(_.content) |
                                   eol.map(_ => "") |
                                   ((anySpaces ~ untilE(eol, min = 0)).! ~ eol))
  def entry[_: P]: P[Macro] = P(withProv(itemMarker ~/ content)
                                .map { case ((id, v), p) =>
                                  Macro(Def, Attributes.a(Attribute(id, v.trim), p))
                                })
  def list[_: P]: P[Macro] = P(withProv(entry.rep(sep = anySpaces, min = 1))
                               .map { case (sm, p) =>
                                 Macro(Def, Attributes.l(sm.flatMap(_.attributes.all), p))
                               })
}
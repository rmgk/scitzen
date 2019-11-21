package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object AttributesParser {
  def start[_ : P]: P[Unit] = P(open | AttributesParserOld.open)

  val  open = "{"
  val  close= "}"

  def equals       [_:P]: P[Unit]      = P(anySpaces ~ ":" ~ anySpaces)
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
    P(open ~ anySpaces ~ listElement.rep(sep = ";" | newline) ~ ";".? ~ anySpaces ~ close)|
    AttributesParserOld.list

  def attrQuotes[_: P]: P[String] = {
    P(anySpaces ~ "\"".rep.!.flatMap { quotes =>
      (("[" ~ untilI("]" ~ quotes))
       | (if (quotes.isEmpty) unquotedValue
          else untilI(quotes)))
    })
  }

}

object AttributesParserOld {
  val  open = "["
  val  close= "]"

  def equals       [_:P]: P[Unit]      = P(verticalSpaces ~ "=" ~ verticalSpaces)
  // https://asciidoctor.org/docs/user-manual/#named-attribute
  // tells us that unquoted attribute values may not contain spaces, but this seems to be untrue in practice
  // however, in the hope of better error messages, we will not allow newlines
  def unquotedValue[_:P]: P[String]    = P(untilE(";" | close | eol)).map(_.trim)
  def value        [_:P]: P[String]    = P(attrQuotes)
  def listDef      [_:P]: P[Attribute] = P(verticalSpaces ~ identifier.! ~ equals ~ value)
                                         .map { case (id, v) => Attribute(id, v) }
  def listValue    [_:P]: P[Attribute] = P(value)
                                         .map(v => Attribute("", v))
  def listElement  [_:P]: P[Attribute] = P(listDef | listValue)
  def listOf[_: P](elem: => P[Attribute], min: Int): P[Seq[Attribute]] = P(verticalSpaces ~ elem.rep(sep = ";" | newline, min = min) ~ ";".? ~ verticalSpaces)
  def list         [_:P]: P[Seq[Attribute]] =
    P(open ~ anySpaces ~ listOf(listElement, min = 0) ~ anySpaces ~ close)

  def lightlist [_: P]: P[Seq[Attribute]] = P(listOf(listDef, min = 1) ~ eol ~ spaceLine)

  def attrQuotes[_: P]: P[String] = {
    P(anySpaces ~ "\"".rep.!.flatMap { quotes =>
      (("[" ~ untilI("]" ~ quotes))
       | (if (quotes.isEmpty) unquotedValue
          else untilI(quotes)))
    })
  }

}

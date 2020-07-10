package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object AttributesParser {
  def start[_: P]: P[Unit] = P(open)

  val open  = "{"
  val close = "}"

  def terminator[_: P] = P(";" | close | eol)
  def unquotedValue[_: P]: P[String] = P(untilE(terminator)).map(_.strip())

  /** value is in the general form of ""[content]"" where all of the quoting is optional,
   * but the closing quote must match the opening quote */
  def value[_: P]: P[String] = {
    P(anySpaces ~ "\"".rep.!.flatMap { quotes =>
      (("[" ~ untilI("]" ~ quotes ~ verticalSpaces ~ &(terminator)))
       | (if (quotes.isEmpty) unquotedValue
          else untilI(quotes ~ verticalSpaces ~ &(terminator))))
    })
  }

  def namedAttribute[_: P]: P[Attribute] =
    P(verticalSpaces ~ identifier.! ~ verticalSpaces ~ "=" ~ value)
    .map { case (id, v) => Attribute(id, v) }

  def positionalAttribute[_: P]: P[Attribute] = P(value).map(v => Attribute("", v))

  def attribute[_: P]: P[Attribute] = P(namedAttribute | positionalAttribute)

  def listOf[_: P](elem: => P[Attribute], min: Int): P[Seq[Attribute]] =
    P(verticalSpaces ~ elem.rep(sep = ";" | newline, min = min) ~ ";".?)

  def braces[_: P]: P[Seq[Attribute]] =
    P(open ~ anySpaces ~ listOf(attribute, min = 0) ~ anySpaces ~ close)

  def noBraces[_: P]: P[Seq[Attribute]] = P(listOf(namedAttribute, min = 1) ~ spaceLine ~ spaceLine)


}

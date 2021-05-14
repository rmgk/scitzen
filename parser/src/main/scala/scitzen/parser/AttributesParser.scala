package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.sast.{Attribute, Inline, Text}

object AttributesParser {
  def start[_: P]: P[Unit] = P(open)

  val open  = "{"
  val close = "}"

  def terminationCheck[_: P]                        = P(&(";" | close | eol))
  val unquotedInlines                               = InlineParsers(s";\n$close", terminationCheck(_), allowEmpty = true)
  def unquotedValue[_: P]: P[(Seq[Inline], String)] = P(unquotedInlines.full)

  /** value is in the general form of ""[content]"" where all of the quoting is optional,
    * but the closing quote must match the opening quote
    */
  def value[_: P]: P[Text] = {
    P(anySpaces ~ ("\"".rep.! ~ "[".!.?).flatMap {
      case ("", None) => unquotedValue
      case (quotes, Some(_)) =>
        InlineParsers("]", _ => s"]$quotes" ~ verticalSpaces ~ terminationCheck, allowEmpty = true).full
      case (quotes, None) =>
        InlineParsers("\"", _ => quotes ~ verticalSpaces ~ terminationCheck, allowEmpty = true).full
      //(("[" ~ untilI("]" ~ quotes ~ verticalSpaces ~ &(terminator)))
      //  | (if (quotes.isEmpty) unquotedValue
      //     else untilI(quotes ~ verticalSpaces ~ &(terminator))))
    }).map(r => scitzen.sast.Text(r._1))
  }

  def namedAttribute[_: P]: P[Attribute] =
    P(verticalSpaces ~ identifier.! ~ verticalSpaces ~ "=" ~ value)
      .map { case (id, v) => scitzen.sast.Attribute(id, v) }

  def positionalAttribute[_: P]: P[Attribute] = P(value).map(v => scitzen.sast.Attribute("", v))

  def attribute[_: P]: P[Attribute] = P(namedAttribute | positionalAttribute)

  def listOf[_: P](elem: => P[Attribute], min: Int): P[Seq[Attribute]] =
    P(verticalSpaces ~ elem.rep(sep = ";" | newline, min = min) ~ ";".?)

  def braces[_: P]: P[Seq[Attribute]] =
    P(open ~ anySpaces ~ listOf(attribute, min = 0) ~ anySpaces ~ close)

  def noBraces[_: P]: P[Seq[Attribute]] = P(listOf(namedAttribute, min = 1) ~ spaceLine ~ spaceLine)

}

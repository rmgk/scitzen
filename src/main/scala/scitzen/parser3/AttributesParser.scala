package scitzen.parser3

import cats.parse.{Numbers, Rfc5234, Parser as P, Parser0 as P0}
import cats.syntax.*
import cats.implicits.*
import cats.parse.Numbers.digits
import cats.parse.Rfc5234.sp
import cats.parse.Parser.*
import scitzen.sast.Prov
import CommonParsers.*
import scitzen.sast.{Attribute, Inline, Text}

object AttributesParser {
  val start: P[Unit] = open

  val open  = "{"
  val close = "}"

  val terminationCheck: P0[Unit] = peek(";" | close | eol)
  val unquotedInlines            = InlineParsers(s";\n$close", terminationCheck, allowEmpty = true)
  val unquotedValue: P[(Seq[Inline], String)] = (unquotedInlines.full)

  /** value is in the general form of ""[content]"" where all of the quoting is optional,
    * but the closing quote must match the opening quote
    */
  val value: P[Text] = {
    (anySpaces.with1 *> ("\"".rep.string ~ "[".string.?).flatMap {
      case ("", None) => unquotedValue
      case (quotes, Some(_)) =>
        InlineParsers("]", (s"]$quotes" ~ verticalSpaces ~ terminationCheck).void, allowEmpty = true).full
      case (quotes, None) =>
        InlineParsers("\"", (quotes ~ verticalSpaces ~ terminationCheck).void, allowEmpty = true).full
      //(("[" ~ untilI("]" ~ quotes ~ verticalSpaces ~ &(terminator)))
      //  | (if (quotes.isEmpty) unquotedValue
      //     else untilI(quotes ~ verticalSpaces ~ &(terminator))))
    }).map(r => scitzen.sast.Text(r._1))
  }

  val namedAttribute: P[Attribute] =
    ((verticalSpaces.with1 *> identifier.string <* verticalSpaces <* "=") ~ value)
      .map { (id: String, v: Text) => scitzen.sast.Attribute(id, v) }

  val positionalAttribute: P[Attribute] = (value).map(v => scitzen.sast.Attribute("", v))

  val attribute: P[Attribute] = (namedAttribute | positionalAttribute)

  def listOf(elem: P[Attribute], min: Int): P0[List[Attribute]] =
    (verticalSpaces *> elem.repSep0(sep = ";" | newline, min = min) <* ";".?)

  val braces: P[List[Attribute]] =
    (open *> anySpaces *> listOf(attribute, min = 0) <* anySpaces <* close)

  val noBraces: P0[List[Attribute]] = (listOf(namedAttribute, min = 1) <* spaceLine <* spaceLine)

}

package scitzen.parser3

import cats.parse.{Numbers, Rfc5234, Parser as P, Parser0 as P0}
import cats.syntax.*
import cats.implicits.*
import cats.parse.Numbers.digits
import cats.parse.Rfc5234.sp
import cats.parse.Parser.*
import scitzen.sast.Prov
import scitzen.parser3.CommonParsers.*
import scitzen.sast.{Attribute, Inline, Text}

object AttributesParser {

  val terminationCheck: P0[Unit] = peek(charIn(s";\n$attrClose").void)

  val unquotedValue: P0[(Seq[Inline], String)] =
    defer0(InlineParsers.full(terminationCheck, allowEmpty = true).withContext("unquoted"))

  /** value is in the general form of ""[content]"" where all of the quoting is optional,
    * but the closing quote must match the opening quote
    */
  val value: P0[Text] = {
    (anySpaces *> (("\"".rep0.string ~ "[".string.?).flatMap {
      case ("", None) => unquotedValue
      case (quotes, Some(_)) =>
        InlineParsers.full((s"]$quotes" ~ verticalSpaces ~ terminationCheck).void, allowEmpty = true)
      case (quotes, None) =>
        InlineParsers.full((quotes ~ verticalSpaces ~ terminationCheck).void, allowEmpty = true)
    }).map(r => scitzen.sast.Text(r._1)))
  }

  val namedAttribute: P[Attribute] =
    (((verticalSpaces.with1 *> identifier.string <* verticalSpaces).backtrack.soft <* "=") ~ value)
      .map { (id: String, v: Text) => scitzen.sast.Attribute(id, v) }.withContext("named")

  val positionalAttribute: P0[Attribute] =
    value.map(v => scitzen.sast.Attribute("", v)).withContext("positional")

  val attribute: P0[Attribute] = (namedAttribute | positionalAttribute).withContext("attribute")

  def listOf(elem: P0[Attribute], min: Int): P0[List[Attribute]] =
    val sep = charIn(";\n")
    ((elem.with1 <* sep).backtrack.rep0(min = min) ~ elem.?)
    .map(_ ++ _).withContext("listof")

  val braces: P[List[Attribute]] =
    (attrOpen *> anySpaces *> listOf(attribute, min = 0) <* anySpaces <* attrClose).withContext("braces")

  val noBraces: P0[List[Attribute]] = (listOf(namedAttribute, min = 1) <* spaceLine).withContext("no braces")

}

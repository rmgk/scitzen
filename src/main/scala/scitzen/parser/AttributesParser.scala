package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.sast.{Attribute, Attributes, Inline, Text}

object AttributesParser {
  def start[_p: P]: P[Unit] = P(open)

  val open  = "{"
  val close = "}"

  def terminationCheck[_p: P] = P(&(";" | close | eol))
  val unquotedInlines         = InlineParsers(s";\n$close", terminationCheck(_), allowEmpty = true)
  def unquotedText[_p: P]: P[(Seq[Inline], String)] = P(unquotedInlines.full)

  /** text is in the general form of ""[content]"" where all of the quoting is optional,
    * but the closing quote must match the opening quote
    */
  def text[_p: P]: P[Text] = {
    P(anySpaces ~ ("\"".rep().! ~ "[".!.?).flatMap {
      case ("", None) => unquotedText
      case (quotes, bracket) =>
        val closing = bracket.fold(quotes)(_ => s"]$quotes")
        InlineParsers(closing.substring(0, 1), _ => closing ~ verticalSpaces ~ terminationCheck, allowEmpty = true).full
    }).map(r => scitzen.sast.Text(r._1))
  }

  def stringValue[_p: P]: P[String] = {
    P(anySpaces ~ ("\"".rep().! ~ "[".!.?).flatMap {
      case ("", None) => CharsWhile(c => c != ';' && c != '}' && c != '\n').?.!
      case (quotes, bracket) =>
        val b = bracket.fold("")(_ => "]")
        untilE(s"$b$quotes" ~ verticalSpaces ~ terminationCheck) ~ s"$b$quotes"
    })
  }

  def namedAttributeValue[p: P]: P[Either[Seq[Attribute], String]] =
    P((anySpaces ~ braces.map(Left.apply)) | stringValue.map(Right.apply))

  def namedAttribute[_p: P]: P[Attribute] =
    P(verticalSpaces ~ identifier.! ~ verticalSpaces ~ "=" ~/ namedAttributeValue)
      .map {
        case (id, Left(attr))   => scitzen.sast.Attribute.Nested(id, Attributes(attr))
        case (id, Right(value)) => scitzen.sast.Attribute.Plain(id, value)
      }

  def positionalAttribute[_p: P]: P[Attribute] = {
    // unclear to me if there is any way to acquire a parsed value AND the parsed string from fastparse
    var hack: Text = null
    P(text).map { v => hack = v }.!.map { value =>
      scitzen.sast.Attribute.Positional(hack, Some(value))
    }
  }

  def attribute[_p: P]: P[Attribute] = P(namedAttribute | positionalAttribute)

  def listOf[_p: P](elem: => P[Attribute], min: Int): P[Seq[Attribute]] =
    P(elem.rep(sep = ";" | newline, min = min) ~ ";".?)

  def braces[_p: P]: P[Seq[Attribute]] =
    P(open ~/ anySpaces ~ listOf(attribute, min = 0) ~ anySpaces ~ close)

  def noBraces[_p: P]: P[Seq[Attribute]] = P(listOf(namedAttribute, min = 1) ~ spaceLine ~ spaceLine)

  def configFile[_p: P]: P[Seq[Attribute]] = P(noBraces.?.map(_.getOrElse(Nil)) ~ anySpaces)

}

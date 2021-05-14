package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.sast.{ScitzenDate, ScitzenTime, ScitzenDateTime}

object TimeParsers {
  def digits[_: P] = CharsWhileIn("0-9")
  def date[_: P]: P[ScitzenDate] =
    P(digits.! ~ "-" ~ digits.! ~ "-" ~ digits.!)
      .map((ScitzenDate.apply _).tupled)
  def time[_: P]: P[ScitzenTime] =
    P(digits.! ~ ":" ~ digits.! ~ ":" ~ digits.! ~ ("." ~ digits).?)
      .map((ScitzenTime.apply _).tupled)
  def timezone[_: P]: P[Unit] = P("+" ~ digits ~ ":" ~ digits)
  def dateTime[_: P]: P[ScitzenDateTime] =
    P(date ~ ((CharsWhile(_.isWhitespace) | "T")
      ~ time
      ~ timezone.?).?)
      .map((ScitzenDateTime.apply _).tupled)

  def parseDate(dateString: String): ScitzenDateTime = {
    fastparse.parse(dateString, dateTime(_)).get.value
  }
}

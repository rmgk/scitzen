package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.sast.{ScitzenDate, ScitzenTime, ScitzenDateTime}

object TimeParsers {
  def digits[_p: P] = CharsWhileIn("0-9")
  def date[_p: P]: P[ScitzenDate] =
    P(digits.! ~ "-" ~ digits.! ~ "-" ~ digits.!)
      .map((ScitzenDate.apply _).tupled)
  def time[_p: P]: P[ScitzenTime] =
    P(digits.! ~ ":" ~ digits.! ~ ":" ~ digits.! ~ ("." ~ digits).?)
      .map((ScitzenTime.apply _).tupled)
  def timezone[_p: P]: P[Unit] = P("+" ~ digits ~ ":" ~ digits)
  def dateTime[_p: P]: P[ScitzenDateTime] =
    P(date ~ ((CharsWhile(_.isWhitespace) | "T")
      ~ time
      ~ timezone.?).?)
      .map((ScitzenDateTime.apply _).tupled)

  def parseDate(dateString: String): ScitzenDateTime = {
    fastparse.parse(dateString, dateTime(_)).get.value
  }
}

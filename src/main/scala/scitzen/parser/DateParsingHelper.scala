package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._

case class ScitzenDate(year: String, month: String, day: String) {
  def full = s"$year-$month-$day"
}
case class ScitzenTime(hour: String, minute: String, second: String) {
  def short = s"$hour:$minute"
}
case class ScitzenDateTime(date: ScitzenDate, timeO: Option[ScitzenTime]) {
  def timeAppend: String   = timeO.fold("")(st => s" ${st.short}")
  def full: String         = s"${date.full}$timeAppend"
  def monthDayTime: String = s"${date.month}-${date.day}$timeAppend"
  def year: String         = date.year
}
object ScitzenDateTime {
  implicit val ord: Ordering[ScitzenDateTime] = Ordering.by(_.full)
}

object DateParsingHelper {
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

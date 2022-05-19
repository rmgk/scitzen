package scitzen.parser3

import cats.parse.{Numbers, Parser0, Rfc5234, Parser as P}
import cats.syntax.*
import cats.implicits.*
import scitzen.sast.{ScitzenDate, ScitzenDateTime, ScitzenTime}
import cats.parse.Numbers.digits
import cats.parse.Rfc5234.sp
import cats.parse.Parser.*

object TimeParsers {

  val colon = P.char(':')

  val date: P[ScitzenDate] =
    (digits <* P.string("-").void, digits <* P.char('-').void, digits).tupled.map(ScitzenDate.apply.tupled)
  val time: P[ScitzenTime] =
    (digits <* colon, digits <* colon, digits <* (P.char('.') *> digits).?).tupled.map(ScitzenTime.apply.tupled)
  val timezone: P[(String, String)] = P.char('+') *> digits ~ (colon *> digits)
  val dateTime: P[ScitzenDateTime] =
    (date ~ ((sp | char('T'))
      *> time
      <* timezone.?).?).map(ScitzenDateTime.apply.tupled)

}

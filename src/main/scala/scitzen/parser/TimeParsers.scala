package scitzen.parser

import de.rmgk.scip.*
import scitzen.sast.{ScitzenDate, ScitzenDateTime, ScitzenTime}

import java.nio.charset.StandardCharsets
import scala.util.Try

object TimeParsers {
  inline def digits: Scip[Boolean] = bpred(b => '0' <= b && b <= '9').rep.min(1)
  inline def date: Scip[ScitzenDate] = Scip {
    val y = digits.str.run
    val m = ("-".all ifso digits.str).opt.run
    val d = ("-".all ifso digits.str).opt.run
    ScitzenDate(y, m, d)
  }
  inline def time = Scip {
    val res = ScitzenTime(
      digits.str.run,
      (":".all ifso digits.str).opt.run,
      (":".all ifso digits.str).opt.run
    )
    (".".all.orFail ~> digits).attempt.run
    res
  }
  inline def timezone = "+".all.ifso(digits <~> (":".all ifso digits))
  inline def dateTime = Scip {
    val sdate = date.run
    val stime = Scip {
      ("T".all or cpred(Character.isWhitespace)).str.run
      val t = time.run
      timezone.attempt.run
      t
    }.opt.run
    ScitzenDateTime(sdate, stime)
  }

  def parseDate(dateString: String): Option[ScitzenDateTime] = {
    Try(Parse.parseResult(dateString.getBytes(StandardCharsets.UTF_8), dateTime)).toOption
  }
}

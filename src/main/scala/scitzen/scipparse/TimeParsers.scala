package scitzen.scipparse

import de.rmgk.scip.*
import scitzen.sast.{ScitzenDate, ScitzenDateTime, ScitzenTime}

import java.nio.charset.StandardCharsets

object TimeParsers {
  val digits: Scip[Unit] = bpred(b => '0' <= b && b <= '9').rep.min(1).orFail
  val date: Scip[ScitzenDate] = Scip {
    val y = digits.str.run
    "-".scip.run
    val m = digits.str.run
    "-".scip.run
    val d = digits.str.run
    ScitzenDate(y, m, d)
  }
  val time = Scip {
    val res = ScitzenTime(
      digits.str.run,
      (":".scip ifso digits.str).run,
      (":".scip ifso digits.str).run
    )
    (".".scip.orFail ~> digits).attempt.run
    res
  }
  val timezone = "+".scip.ifso(digits <~> (":".scip ifso digits))
  val dateTime = Scip {
    val sdate = date.run
    val stime = Scip {
      choice("T".scip.orFail, cpred(Character.isWhitespace).orFail).str.run
      val t = time.run
      timezone.attempt.run
      t
    }.opt.run
    ScitzenDateTime(sdate, stime)
  }

  def parseDate(dateString: String): ScitzenDateTime = {
    Parse.parseResult(dateString.getBytes(StandardCharsets.UTF_8), dateTime)
  }
}

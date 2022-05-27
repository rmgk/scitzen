package scitzen.scipparse

import de.rmgk.scip.*
import scitzen.sast.{ScitzenDate, ScitzenDateTime, ScitzenTime}

object TimeParsers {
  val digits: Scip[Unit] = whileRange('0', '9')
  val date: Scip[ScitzenDate] = Scip {
    val y = digits.!.run
    "-".scip.run
    val m = digits.!.run
    "-".scip.run
    val d = digits.!.run
    ScitzenDate(y, m, d)
  }
  val time = Scip {
    val res = ScitzenTime(
      digits.!.run,
      { ":".scip.run; digits.!.run },
      { ":".scip.run; digits.!.run }
    )
    (".".scip ~ digits).?.run
    res
  }
  val timezone = "+".scip ~ digits ~ ":".scip ~ digits
  val dateTime = Scip {
    val sdate = date.run
    val stime = Scip {
      println(scx.index)
      println(s"choice: »${choice("T".scip, whitespace).!.run}«")
      println(scx.index)
      val t = time.run
      timezone.?.run
      t
    }.opt.run

    println(s"$stime, »${scx.input.view.slice(scx.index, scx.index + 42).str}«")

    ScitzenDateTime(sdate, stime)
  }

}

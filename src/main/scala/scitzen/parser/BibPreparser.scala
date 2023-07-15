package scitzen.parser

import de.rmgk.scip.*

import scala.annotation.tailrec

case class Biblet(format: String, id: String, full: Array[Byte])

object BibPreparser {

  def entry: Scip[Biblet] = Scip:
    until("@".any).run
    val start = scx.index
    scx.next
    val format = until("{".any).min(1).str.run
    scx.next
    val id = until(",".any).min(1).str.run.trim

    Predef.require(!id.contains(' '), s"something went wrong when parsing a bibfile for id: ${id}")

    var opened = 0

    @tailrec
    def rec(): Unit =
      until("{}\\".any).min(0).orFail.run
      if "}".any.run
      then
        if opened > 0
        then
          opened -= 1
          rec()
        else ()
      else if "\\".any.run
      then
        scx.next
        rec()
      else if "{".any.run
      then
        opened += 1
        rec()

    rec()
    val full = scx.input.slice(start, scx.index + 1)
    Biblet(format = format, id = id, full = full)
}

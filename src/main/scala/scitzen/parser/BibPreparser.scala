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

    @tailrec
    def rec(opened: Int): Unit =
      until("{}\\".any).min(0).orFail.run
      () match
        case _ if "}".any.run =>
          if opened > 0
          then
            rec(opened - 1)
          else ()
        case _ if "\\".any.run =>
          scx.next
          rec(opened)
        case _ if "{".any.run =>
          rec(opened + 1)

    rec(0)
    val full = scx.input.slice(start, scx.index + 1)
    Biblet(format = format, id = id, full = full)
}
package scitzen.parser

import de.rmgk.scip.*

import java.io.{ByteArrayInputStream, InputStream}
import scala.annotation.tailrec

case class Inputrange(start: Int, end: Int, full: Array[Byte]):
  def inputstream: InputStream =
    new ByteArrayInputStream(full, start, end - start)
case class Biblet(format: String, id: String, full: Inputrange)


object BibPreparser {

  def entry: Scip[Biblet] = Scip:
    until("@".any).run
    val start = scx.index
    scx.next
    val format = until("{".any).min(1).str.run
    scx.next
    val id = until(",".any).min(1).str.run.trim

    Predef.require(!id.contains(' '), s"something went wrong when parsing a bibfile for id: ${id}")

    @tailrec def rec(opened: Int): Unit =
      until("{}\\".any).min(0).orFail.run
      if "\\".any.run then { scx.next; return rec(opened) }
      if "{".any.run then return rec(opened + 1)
      if "}".any.run && opened > 0 then return rec(opened - 1)
      ()
    rec(0)

    Biblet(format = format, id = id, full = Inputrange(start, scx.index + 1, scx.input))
}

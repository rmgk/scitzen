package scitzen.parser

import de.rmgk.scip.*

import java.io.{ByteArrayInputStream, InputStream, SequenceInputStream}
import java.nio.charset.StandardCharsets
import scala.annotation.{nowarn, tailrec}

case class Inputrange(start: Int, end: Int, full: Array[Byte]):
  def inputstream: InputStream =
    new ByteArrayInputStream(full, start, end - start)
case class Biblet(format: String, id: String, inner: Inputrange, alias: List[String]):
  def inputstream: InputStream =
    SequenceInputStream(ByteArrayInputStream(header.getBytes(StandardCharsets.UTF_8)), inner.inputstream)
  def header: String = s"@$format{$id,"

object BibPreparser {

  def entry: Scip[Biblet] = Scip:
    until("@".any).run: @nowarn
    scx.next: @nowarn
    val format = until("{".any).min(1).str.run
    scx.next: @nowarn
    val id = until(",".any).min(1).str.run.trim
    scx.next: @nowarn
    val startInner = scx.index

    Predef.require(!id.contains(' '), s"something went wrong when parsing a bibfile for id: ${id}")

    @tailrec def rec(opened: Int): Unit =
      until("{}\\".any).min(0).orFail.run
      if "\\".any.run then { scx.next: @nowarn; return rec(opened) }
      if "{".any.run then return rec(opened + 1)
      if "}".any.run && opened > 0 then return rec(opened - 1)
      ()
    rec(0)

    Biblet(format, id, Inputrange(startInner, scx.index + 1, scx.input), alias = List(id))
}

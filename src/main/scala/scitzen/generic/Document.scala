package scitzen.generic

import scitzen.sast.{Directive, Prov, Sast}
import scitzen.compat.Logging.scribe

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.collection.immutable.ArraySeq
import scala.util.control.NonFatal

/** A document represents a single, on disk, text file that has been successfully parsed */
case class Document(file: Path, content: Array[Byte], sast: List[Sast]):
  lazy val reporter: FileReporter = new FileReporter(file, content)

object Document:
  def apply(file: Path): Document =
    val content = Files.readAllBytes(file)
    try
      val sast = scitzen.parser.Parse.documentUnwrap(content, Prov(0, content.length))
      Document(file, content, sast.toList)
    catch
      case NonFatal(e) =>
        scribe.error(s"error while parsing $file")
        throw e

trait Reporter:
  def apply(im: Directive): String = apply(im.prov)
  def apply(prov: Prov): String

final class FileReporter(file: Path, content: Array[Byte]) extends Reporter:
  lazy val newLines: Seq[Int] =
    def findNL(idx: Int, found: List[Int]): Array[Int] =
      val res = content.indexOf('\n', idx + 1)
      if res >= 0 then findNL(res, res :: found)
      else found.toArray.reverse

    ArraySeq.unsafeWrapArray(findNL(-1, Nil))

  def indexToPosition(idx: Int): (Int, Int) =
    val ip     = newLines.search(idx).insertionPoint
    val offset = if ip == 0 then 0 else newLines(ip - 1)
    (ip + 1, idx - offset)

  override def apply(prov: Prov): String =
    val pos = indexToPosition(prov.start)
    s" at »${Path.of("").toAbsolutePath.relativize(file)}:" +
    s"${pos._1}:${pos._2}«"

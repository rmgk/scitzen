package scitzen.generic

import better.files.File
import cats.implicits._
import scitzen.parser.Parse
import scitzen.sast.{Macro, Prov, Sast}

import scala.collection.immutable.ArraySeq
import scala.util.control.NonFatal


/** A document represents a single, on disk, textfile that has been successfully parsed */
case class Document(file: File, content: String, sast: List[Sast]) {
  lazy val reporter: FileReporter = new FileReporter(file, content)
}

object Document {
  def apply(file: File): Document = {
    val content = file.contentAsString
    try {
      val sast = Parse.documentUnwrap(content, Prov(0, content.length))
      Document(file, content, sast.toList)
    } catch {
      case NonFatal(e) =>
        scribe.error(s"error while parsing $file")
        throw e
    }
  }
}

trait Reporter {
  def apply(im: Macro): String = apply(im.attributes.prov)
  def apply(prov: Prov): String
}

final class FileReporter(file: File, content: String) extends Reporter {
  lazy val newLines: Seq[Int] = {
    def findNL(idx: Int, found: List[Int]): Array[Int] = {
      val res = content.indexOf('\n', idx + 1)
      if (res >= 0) findNL(res, res :: found)
      else found.toArray.reverse
    }

    ArraySeq.unsafeWrapArray(findNL(-1, Nil))
  }

  def indexToPosition(idx: Int): (Int, Int) = {
    val ip     = newLines.search(idx).insertionPoint
    val offset = if (ip == 0) 0 else newLines(ip - 1)
    (ip + 1, idx - offset)
  }

  override def apply(prov: Prov): String = {
    val pos = indexToPosition(prov.start)
    s" at »${File.currentWorkingDirectory.relativize(file)}:" +
      s"${pos._1}:${pos._2}«"
  }
}

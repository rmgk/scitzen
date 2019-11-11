package scitzen.generic

import java.nio.file.Path

import better.files.File
import cats.data.NonEmptyList
import scitzen.generic.Sast.TLBlock
import scitzen.parser.Prov

import scala.util.control.NonFatal


case class DocumentDiscovery(sourcePaths: List[File]) {

  val fileEnding  = "scim"
  val globPattern = "**." + fileEnding

  lazy val sourceDirectories: List[File] = sourcePaths.filter(_.isDirectory)
  lazy val sourceFiles      : List[File] = sourcePaths.flatMap {
    case f if f.isRegularFile => List(f)
    case f if f.isDirectory   =>
      f.glob(globPattern).toList
      // extension also checks if the file is a regular file and exists
      .filter(f => f.extension(includeDot = false, toLowerCase = true).contains(fileEnding))
  }
}

object DocumentDiscovery {
  def apply(nonEmptyList: NonEmptyList[Path]): DocumentDiscovery =
    DocumentDiscovery(nonEmptyList.map(File(_)).toList)
}


final case class ParsedDocument(file: File, content: String, blocks: Seq[TLBlock], sdoc: Sdoc, newLines: Seq[Int]) {
  def indexToPosition(idx: Int): (Int, Int) = {
    val ip = scala.collection.Searching.search(newLines).search(idx).insertionPoint
    val offset = if(ip == 0) 0 else newLines(ip - 1)
    (ip + 1, idx - offset)
  }
}
object ParsedDocument {
  def apply(file: File): ParsedDocument = {
    val content = file.contentAsString
    def findNL(idx: Int, found: List[Int]): Array[Int] = {
      val res = content.indexOf('\n', idx + 1)
      if (res >= 0) findNL(res, res :: found)
      else found.toArray.reverse
    }
    try {
      val sast = SastConverter().documentString(content, Prov(0, content.length))
      val sdoc = Sdoc(sast)
      ParsedDocument(file, content, sast, sdoc, findNL(-1, Nil))
    } catch {
      case NonFatal(e) =>
        scribe.error(s"error while parsing $file")
        throw e
    }
  }
}
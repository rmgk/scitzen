package scitzen.generic

import java.nio.file.Path

import better.files.File
import cats.data.NonEmptyList
import scitzen.generic.Sast.TLBlock


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


final case class ParsedDocument(file: File, content: String, blocks: Seq[TLBlock], sdoc: Sdoc)
object ParsedDocument {
  def apply(file: File): ParsedDocument = {
    val content = file.contentAsString
    val sast = SastConverter().documentString(content)
    val sdoc = Sdoc(sast)
    ParsedDocument(file, content, sast, sdoc)
  }
}
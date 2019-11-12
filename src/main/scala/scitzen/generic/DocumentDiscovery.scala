package scitzen.generic

import java.nio.file.Path

import better.files.File
import cats.data.NonEmptyList


case class DocumentDiscovery(sourcePaths: List[File]) {

  val fileEnding  = "scim"
  val globPattern = "**." + fileEnding

  lazy val sourceDirectories: List[File] = sourcePaths.filter(_.isDirectory)
  lazy val sourceFiles      : List[File] = sourcePaths.filter(_.exists).flatMap {
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



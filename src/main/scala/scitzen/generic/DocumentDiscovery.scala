package scitzen.generic

import java.nio.file.Path

import better.files.File
import cats.data.{Chain, NonEmptyList}


case class DocumentDiscovery(sourcePaths: List[File]) {

  val fileEnding  = "scim"
  val globPattern = "**." + fileEnding

  def isScim(f: File) = f.extension(includeDot = false, toLowerCase = true).contains(fileEnding)

  lazy val sourceFiles      : List[File] = sourcePaths.filter(_.exists).flatMap {
    case f if f.isRegularFile             => List(f)
    case outerDir if outerDir.isDirectory =>
      def rec(file: File): Chain[File] = file match {
        case f if isScim(f) && !f.isHidden => Chain(f)
        case dir if dir.isDirectory && !dir.isHidden => Chain.fromSeq(dir.children.toList).flatMap(rec)
        case _ => Chain.empty
      }
      rec(outerDir).toList
  }
}

object DocumentDiscovery {
  def apply(nonEmptyList: NonEmptyList[Path]): DocumentDiscovery =
    DocumentDiscovery(nonEmptyList.map(File(_)).toList)
}



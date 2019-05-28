package scitzen.cli

import better.files.File
import scitzen.generic.{DocumentManager, ParsedDocument}

case class Project(root: File, singleSource: Option[File] = None) {
  val projectDir: File = root / Project.scitzenfolder
  val cacheDir  : File = projectDir / "cache"
  lazy val sources        : List[File]           = singleSource match {
    case None => Project.discoverSources(root)
    case Some(source) => List(source)
  }
  lazy val documents      : List[ParsedDocument] = sources.map(ParsedDocument.apply)
  lazy val documentManager: DocumentManager      = DocumentManager.resolveIncludes(new DocumentManager(documents))
  val outputdir: File = projectDir / "output"
}

object Project {
  val scitzenfolder: String = ".scitzen"
  def findRoot(source: File): Option[File] = {
    if (( source / scitzenfolder).isDirectory) Some(source)
    else source.parentOption.flatMap(findRoot)
  }
  def fromSource(file: File): Project = {
    val root = findRoot(file)
    val source =
      if (isScim(file)) Some(file)
      else if (file.isDirectory) {
        file.collectChildren(isScim, 1).toList match {
          case List(single) => Some(single)
          case other => None
        }
      } else None
    Project(root.getOrElse(file / "meh"), source)
  }

  def isScim(c: File): Boolean =
    c.isRegularFile &&
    c.extension(includeDot = false, toLowerCase = true).contains(fileEnding)

  val fileEnding = "scim"
  def discoverSources(source: File): List[File] = {
    import scala.collection.JavaConverters._
    source match {
      case f if f.isRegularFile => List(f)
      case f if f.isDirectory   =>
        f.collectChildren{ c =>
          isScim(c) &&
          !f.relativize(c).iterator().asScala.exists {_.toString.startsWith("_") }
        }.toList
    }
  }

}
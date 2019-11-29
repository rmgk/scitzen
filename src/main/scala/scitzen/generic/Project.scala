package scitzen.generic

import java.nio.file.{Path, Paths}

import better.files.File

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


  def findDoc(anchor: File, pathString: String): Option[ParsedDocument] = {
    val path: File = findFile(anchor, pathString)
    documentManager.byPath.get(path)
  }

  def findFile(anchor: File, pathString: String): File = {
    val rawPath = Paths.get(pathString)
    if (rawPath.isAbsolute) File(root, Paths.get("/").relativize(rawPath).toString)
    else anchor / pathString
  }

  def relativize(file: File): Option[Path] =
    if (root.isParentOf(file)) Some(root.relativize(file))
    else None
}

object Project {
  val scitzenfolder: String = "scitzen"
  def findRoot(source: File): Option[File] = {
    if (( source / scitzenfolder).isDirectory) Some(source)
    else source.parentOption.flatMap(findRoot)
  }
  def fromSource(file: File): Option[Project] = {
    findRoot(file) match {
      case None       => scribe.info(
        s"""could not find project root containing the directory `$scitzenfolder`""")
        None
      case Some(root) =>
        scribe.info(s"found root at $root")
        val source = {
          if (isScim(file) && root.listRecursively.contains(file)) Some(file)
          else {
            root.collectChildren(isScim, 1).toList match {
              case List(single) => Some(single)
              case other        => None
            }
          }
        }

        Some(Project(root, source))
    }
  }

  def isScim(c: File): Boolean =
    c.isRegularFile &&
    c.extension(includeDot = false, toLowerCase = true).contains(fileEnding)

  val fileEnding = "scim"
  def discoverSources(source: File): List[File] = {
    import scala.jdk.CollectionConverters._
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

package scitzen.generic

import java.nio.file.Paths

import better.files.File
import scitzen.generic.Project.ProjectConfig
import toml.Codecs._

case class Project(root: File, config: ProjectConfig) {

  val cacheDir: File  = root / config.cache
  val outputdir: File = root / config.output
  val nlpdir: File    = root / config.stopwords

  def resolveUnchecked(currentWorkingDirectory: File, pathString: String): File = {
    val rawPath = Paths.get(pathString)
    val res =
      if (rawPath.isAbsolute) File(root, Paths.get("/").relativize(rawPath).toString)
      else currentWorkingDirectory / pathString
    scribe.trace(s"lookup of $pathString in $currentWorkingDirectory was $res")
    res
  }

  /** Does a project global file local resolve of the given path.
    * Ensures that only files in the current project are accessed
    */
  def resolve(currentWorkingDirectory: File, pathString: String): Option[File] = {
    val res = resolveUnchecked(currentWorkingDirectory, pathString)
    Some(res).filter(p => root.isParentOf(p) && p.isRegularFile)
  }

}

object Project {

  case class ProjectConfig(
      output: String = "out",
      cache: String = "cache",
      stopwords: String = "scitzen",
      format: List[String] = Nil,
      outputType: List[String] = Nil,
      revealTemplate: Option[String] = None,
      definitions: Map[String, String] = Map.empty,
      texTemplate: Option[String] = None
  )

  val scitzenconfig: String = "scitzen.toml"

  def findRoot(source: File): Option[File] = {
    if ((source / scitzenconfig).isRegularFile) Some(source)
    else source.parentOption.flatMap(findRoot)
  }

  def fromSource(file: File): Option[Project] = {
    if (isScim(file)) {
      findRoot(file) match {
        case None       => Some(Project(file.parent, ProjectConfig()))
        case Some(file) => fromConfig(file)
      }
    } else if (file.isDirectory) {
      if ((file / scitzenconfig).isRegularFile) {
        fromConfig(file)
      } else Some(Project(file, ProjectConfig()))
    } else None
  }

  def fromConfig(file: File): Option[Project] = {
    locally(stringCodec) // make tools believe the import is used
    toml.Toml.parseAs[ProjectConfig]((file / scitzenconfig).contentAsString) match {
      case Right(value) => Some(Project(file, value))
      case Left((addr, mesg)) =>
        val errormessage = s"could not parse config:\n$mesg\nat $addr"
        scribe.error(errormessage)
        None
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
      case f if f.isDirectory =>
        f.collectChildren { c =>
          isScim(c) &&
          !f.relativize(c).iterator().asScala.exists { _.toString.startsWith(".") }
        }.toList
    }
  }

}

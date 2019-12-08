package scitzen.generic

import java.nio.file.Paths

import better.files.File
import scitzen.generic.Project.ProjectConfig
import toml.Codecs._

case class Project(root: File, config: ProjectConfig, singleSource: Option[File] = None) {


  val cacheDir  : File = root / config.cache
  lazy val sources        : List[File]           = singleSource match {
    case None         => Project.discoverSources(root)
    case Some(source) => List(source)
  }
  lazy val documents      : List[ParsedDocument] = sources.map(ParsedDocument.apply)
  lazy val documentManager: DocumentManager      = DocumentManager.resolveIncludes(new DocumentManager(documents))
  val outputdir: File = root / config.output
  val nlpdir   : File = root / config.stopwords



  def findDoc(currentWorkingDirectory: File, pathString: String): Option[ParsedDocument] = {
    resolve(currentWorkingDirectory, pathString)
    .flatMap(documentManager.byPath.get)
  }

  def resolve(currentWorkingDirectory: File, pathString: String): Option[File] = {
    val rawPath = Paths.get(pathString)
    val res     =
      if (rawPath.isAbsolute) File(root, Paths.get("/").relativize(rawPath).toString)
      else currentWorkingDirectory / pathString
    scribe.trace(s"lookup of $pathString in $currentWorkingDirectory was $res")
    Some(res).filter(p => root.isParentOf(p) && p.isRegularFile)
  }


}

object Project {

  case class ProjectConfig
  (output: String = "output",
   cache: String = "cache",
   stopwords: String = "scitzen")

  val scitzendir: String = "scitzen"
  val scitzenconfig: String = "scitzen.toml"
  def findRoot(source: File): Option[File] = {
    if (( source / scitzenconfig).isRegularFile) Some(source)
    else source.parentOption.flatMap(findRoot)
  }
  def fromSource(file: File): Option[Project] = {
    findRoot(file) match {
      case None       => scribe.info(
        s"""could not find project root containing a config `$scitzenconfig`""")
        None
      case Some(root) =>
        val config =
          toml.Toml.parseAs[ProjectConfig]((root / scitzenconfig).contentAsString) match {
            case Right(value) => value
            case Left((addr, mesg)) =>
              val errormessage = s"could not parse config:\n$mesg\nat $addr"
              scribe.error(errormessage)
              throw new IllegalArgumentException(errormessage)
          }
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

        Some(Project(root, config, source))
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

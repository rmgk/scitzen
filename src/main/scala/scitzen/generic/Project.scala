package scitzen.generic

import better.files.File
import scitzen.bibliography.{BibEntry, Bibtex}
import scitzen.sast.{Prov, Text}
import scitzen.compat.Logging.scribe

import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}

case class Project(root: File, config: ProjectConfig, definitions: Map[String, Text]):

  val cacheDir: File  = root / config.cache
  val outputdir: File = root / config.output
  val nlpdir: File    = root / config.stopwords

  lazy val bibfile: Option[File]               = config.bibliography.flatMap(s => resolve(root, s))
  lazy val bibliography: Map[String, BibEntry] = Bibtex.makeBib(this)

  def relativizeToProject(target: File): Path =
    Paths.get("/").resolve(root.relativize(target))

  def resolveUnchecked(currentWorkingDirectory: File, pathString: String): File =
    val rawPath = Paths.get(pathString)
    val res =
      if rawPath.isAbsolute then File(root, Paths.get("/").relativize(rawPath).toString)
      else currentWorkingDirectory / pathString
    scribe.trace(s"lookup of $pathString in $currentWorkingDirectory was $res")
    res

  /** Does a project global file local resolve of the given path.
    * Ensures that only files in the current project are accessed
    */
  def resolve(currentWorkingDirectory: File, pathString: String): Option[File] =
    val res = resolveUnchecked(currentWorkingDirectory, pathString)
    Some(res).filter(p => root.isParentOf(p) && p.isRegularFile)

object Project:
  val scitzenconfig: String = "scitzen.project/config"

  def findRoot(source: File): Option[File] =
    if (source / scitzenconfig).isRegularFile then Some(source)
    else source.parentOption.flatMap(findRoot)

  def fromSource(file: File): Option[Project] =
    if isScim(file) then
      findRoot(file) match
        case None       => Some(Project(file.parent, ProjectConfig.parse("a=b".getBytes(StandardCharsets.UTF_8)), Map.empty))
        case Some(file) => fromConfig(file)
    else if file.isDirectory then
      if (file / scitzenconfig).isRegularFile then
        fromConfig(file)
      else Some(Project(file, ProjectConfig.parse("c=d".getBytes(StandardCharsets.UTF_8)), Map.empty))
    else None

  def fromConfig(file: File): Option[Project] =
    val configContent = (file / scitzenconfig).byteArray
    val value         = ProjectConfig.parse(configContent)
    val definitions = value.definitions.view.map { (k, v) =>
      k -> Text(scitzen.scipparse.Parse.inlineUnwrap(v.getBytes(StandardCharsets.UTF_8), Prov()))
    }.toMap
    Some(Project(file, value, definitions))

  def isScim(c: File): Boolean =
    c.isRegularFile &&
      c.extension(includeDot = false, toLowerCase = true).contains(fileEnding)

  val fileEnding = "scim"
  def discoverSources(source: File): List[File] =
    import scala.jdk.CollectionConverters.*
    source match
      case f if f.isRegularFile => List(f)
      case f if f.isDirectory =>
        f.collectChildren { c =>
          isScim(c) &&
          !f.relativize(c).iterator().asScala.exists { _.toString.startsWith(".") }
        }.toList

  def directory(root: File): DocumentDirectory =
    scribe.debug(s"discovering sources in ${root}")
    val sources: List[File] = Project.discoverSources(root)
    scribe.debug(s"parsing ${sources.length} documents")
    val documents = sources.map(Document.apply)
    DocumentDirectory(documents)

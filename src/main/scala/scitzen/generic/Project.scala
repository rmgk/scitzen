package scitzen.generic

import scitzen.sast.{Prov, Text}
import scitzen.compat.Logging.scribe

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using

case class Project(root: Path, config: ProjectConfig, definitions: Map[String, Text]):

  val cacheDir: Path = config.cache match
    case None    => root resolve config.output resolve "cache"
    case Some(p) => root resolve p
  val outputdir: Path = root resolve config.output

  lazy val bibfile: Option[Path]  = config.bibliography.flatMap(s => resolve(root, s))
  lazy val bibfileDBLPcache: Path = cacheDir.resolve("dblpcache.bib")

  def relativizeToProject(target: Path): Path =
    Path.of("/").resolve(root.relativize(target))

  def resolveUnchecked(currentWorkingDirectory: Path, pathString: String): Path =
    val rawPath = Path.of(pathString)
    val res =
      if rawPath.isAbsolute then root.resolve(Path.of("/").relativize(rawPath))
      else currentWorkingDirectory resolve pathString
    scribe.trace(s"lookup of $pathString in $currentWorkingDirectory was $res")
    res

  /** Does a project global file local resolve of the given path.
    * Ensures that only files in the current project are accessed
    */
  def resolve(currentWorkingDirectory: Path, pathString: String): Option[Path] =
    val res = resolveUnchecked(currentWorkingDirectory, pathString)
    Some(res).filter(p => p.startsWith(root) && Files.isRegularFile(p))

object Project:
  val scitzenconfig: String = "scitzen.config"

  def findRoot(source: Path): Option[Path] =
    if Files.isRegularFile(source resolve scitzenconfig) then Some(source)
    else Option(source.getParent).flatMap(findRoot)

  def fromSource(file: Path): Option[Project] =
    findRoot(file) match
      case None =>
        val adHocRoot = if Files.isDirectory(file) then file else file.getParent
        Some(Project(adHocRoot, ProjectConfig.parse("a=b".getBytes(StandardCharsets.UTF_8)), Map.empty))
      case Some(file) => fromConfig(file)

  def fromConfig(file: Path): Option[Project] =
    val configContent = Files.readAllBytes(file resolve scitzenconfig)
    val value         = ProjectConfig.parse(configContent)
    val definitions = value.definitions.view.map { (k, v) =>
      k -> Text(scitzen.parser.Parse.inlineUnwrap(v.getBytes(StandardCharsets.UTF_8), Prov()))
    }.toMap
    Some(Project(file, value, definitions))

  def isScim(c: Path): Boolean =
    Files.isRegularFile(c) &&
    c.getFileName.toString.endsWith(".scim")

  /** returs a list of .scim files starting at `source`. Does not follow symlinks and ignores files and folders starting with a . */
  def discoverSources(source: Path): List[Path] =
    import scala.jdk.CollectionConverters.*
    def hasDotComponent(c: Path): Boolean =
      source.relativize(c).iterator().asScala.exists { _.toString.startsWith(".") }
    Using(Files.walk(source)) { stream =>
      stream.iterator().asScala.filter { c =>
        isScim(c) && !hasDotComponent(c)
      }.toList
    }.get

  def directory(root: Path): DocumentDirectory =
    scribe.debug(s"discovering sources in ${root}")
    val sources: List[Path] = Project.discoverSources(root)
    scribe.debug(s"parsing ${sources.length} documents")
    val documents = sources.map(Document.apply)
    DocumentDirectory(documents)

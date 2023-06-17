package scitzen.generic

import scitzen.compat.Logging.scribe
import scitzen.sast.Text

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class ProjectPath private (val project: Project, val relative: Path):
  val absolute: Path = project.root.resolve(relative)
  val directory: Path = absolute.getParent

  override def hashCode(): Int = relative.hashCode()
  override def equals(obj: Any): Boolean = obj match
    case other: ProjectPath => relative == other.relative
    case _ => false
  override def toString: String = s"Path(${relative.toString})"

object ProjectPath:
  def apply(project: Project, target: Path) =
    val normalized = project.root.resolve(target).normalize()
    require(normalized.startsWith(project.root), s"»$target« is not within »$project.root«")
    require(Files.isRegularFile(normalized) || Files.notExists(normalized), s"only regular files may be documents: $normalized")
    new ProjectPath(project, project.root.relativize(normalized))

case class Project(root: Path, config: ProjectConfig, definitions: Map[String, Text]):

  val cacheDir: Path = config.cache match
    case None    => root resolve config.output resolve "cache"
    case Some(p) => root resolve p
  val outputdir: Path = root resolve config.output

  val outputdirWeb: Path = outputdir resolve "web"
  val outputdirPdf: Path = outputdir resolve "pdfs"

  val htmlPaths: HtmlPathManager = HtmlPathManager(this)

  lazy val bibfile: Option[ProjectPath]  = config.bibliography.flatMap(s => resolve(root, s))
  lazy val bibfileDBLPcache: ProjectPath =  asProjectPath(cacheDir.resolve("dblpcache.bib"))

  def asProjectPath(target: Path): ProjectPath = ProjectPath.apply(this, target)

  def resolveUnchecked(currentWorkingDirectory: Path, pathString: String): Path =
    val rawPath = Path.of(pathString)
    val res =
      if rawPath.isAbsolute then root.resolve(Path.of("/").relativize(rawPath))
      else currentWorkingDirectory resolve pathString
    scribe.trace(s"lookup of $pathString in $currentWorkingDirectory was $res")
    res.normalize()

  /** Does a project global file local resolve of the given path.
    * Ensures that only files in the current project are accessed
    */
  def resolve(currentWorkingDirectory: Path, pathString: String): Option[ProjectPath] =
    val res = resolveUnchecked(currentWorkingDirectory, pathString)
    Some(res).filter(p => p.startsWith(root) && ( Files.isRegularFile(p) || Files.notExists(p))).map{ p =>
      ProjectPath(this, root.relativize(p))
    }

object Project:
  val scitzenconfig: String = "scitzen.config"

  def findRoot(source: Path): Option[Path] =
    if Files.isRegularFile(source resolve scitzenconfig) then Some(source)
    else Option(source.getParent).flatMap(findRoot)

  def fromSource(file: Path): Option[Project] =
    findRoot(file.toAbsolutePath) match
      case None =>
        val adHocRoot = if Files.isDirectory(file) then file else file.getParent
        Some(Project(adHocRoot, ProjectConfig.parse("a=b".getBytes(StandardCharsets.UTF_8)), Map.empty))
      case Some(file) => fromConfig(file)

  def fromConfig(file: Path): Option[Project] =
    val configContent = Files.readAllBytes(file resolve scitzenconfig)
    val value         = ProjectConfig.parse(configContent)
    val definitions = value.definitions.view.map { (k, v) =>
      k -> Text(scitzen.parser.Parse.inlineUnwrap(v.getBytes(StandardCharsets.UTF_8)))
    }.toMap
    Some(Project(file, value, definitions))



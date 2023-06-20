package scitzen.generic

import scitzen.compat.Logging.scribe
import scitzen.sast.Text

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class ProjectPath private (val project: Project, val relativeToProject: Path):
  val absolute: Path        = project.root.resolve(relativeToProject)
  val directory: Path       = absolute.getParent
  val projectAbsolute: Path = Path.of("/").resolve(relativeToProject)

  override def hashCode(): Int = relativeToProject.hashCode()
  override def equals(obj: Any): Boolean = obj match
    case other: ProjectPath => relativeToProject == other.relativeToProject
    case _                  => false
  override def toString: String = s"Path(${relativeToProject.toString})"

object ProjectPath:
  def apply(project: Project, target: Path) =
    val normalized = project.root.resolve(target).normalize()
    require(normalized.startsWith(project.root), s"»$target« is not within »$project.root«")
    require(
      Files.isRegularFile(normalized) || Files.notExists(normalized),
      s"only regular files may be documents: $normalized"
    )
    new ProjectPath(project, project.root.relativize(normalized))

case class Project private (root: Path, config: ProjectConfig, definitions: Map[String, Text]):

  val cacheDir: Path = config.cache match
    case None    => root resolve config.output resolve "cache"
    case Some(p) => root resolve p
  val outputdir: Path = root resolve config.output

  val outputdirWeb: Path = outputdir resolve "web"
  val outputdirPdf: Path = outputdir resolve "pdfs"

  val htmlPaths: HtmlPathManager = HtmlPathManager(this)

  lazy val bibfile: Option[ProjectPath]  = config.bibliography.flatMap(s => resolve(root, s))
  lazy val bibfileDBLPcache: ProjectPath = asProjectPath(cacheDir.resolve("dblpcache.bib"))

  def cachePath(target: Path): ProjectPath     = ProjectPath(this, cacheDir.resolve(target))
  def asProjectPath(target: Path): ProjectPath = ProjectPath.apply(this, target)

  def resolveUnchecked(currentWorkingDirectory: Path, rawPath: Path): Path =
    val res =
      if rawPath.isAbsolute then root.resolve(Path.of("/").relativize(rawPath))
      else currentWorkingDirectory resolve rawPath
    scribe.trace(s"lookup of $rawPath in $currentWorkingDirectory was $res")
    res.normalize()

  /** Does a project global file local resolve of the given path.
    * Ensures that only files in the current project are accessed
    */
  def resolve(currentWorkingDirectory: Path, pathString: Path): Option[ProjectPath] =
    val res = resolveUnchecked(currentWorkingDirectory, pathString)
    Some(res).filter(p => p.startsWith(root) && (Files.isRegularFile(p) || Files.notExists(p))).map { p =>
      ProjectPath(this, root.relativize(p))
    }
  def resolve(currentWorkingDirectory: Path, pathString: String): Option[ProjectPath] =
    resolve(currentWorkingDirectory, Path.of(pathString))

object Project:
  val scitzenconfig: String = "scitzen.config"

  def apply(root: Path, config: ProjectConfig, definitions: Map[String, Text]) =
    val absolutelyNormal = root.toAbsolutePath.normalize()
    require(Files.isDirectory(absolutelyNormal), "project root must be a directory")
    new Project(absolutelyNormal, config, definitions)

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

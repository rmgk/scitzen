package scitzen.project

import scitzen.cli.Logging.cli
import scitzen.resources.ImagePaths

import java.nio.file.{FileVisitOption, Files, Path}
import scala.util.Using

case class ProjectPath private (absolute: Path)(project: Project):
  private def relativeToProject               = project.root.relativize(absolute)
  def projectAbsolute: Path                   = Path.of("/").resolve(relativeToProject)
  def resolve(p: String): Option[ProjectPath] = project.resolve(absolute.getParent, p)

object ProjectPath:
  def apply(project: Project, target: Path) =
    // we store directories as having `/.` at the end, to trick `.getParent` into returning the current directory
    val normalized =
      val plain = project.root.resolve(target).normalize()
      if target.endsWith(".") then plain.resolve(".")
      else plain
    require(
      normalized.startsWith(project.root) || normalized.startsWith(project.outputdir) || normalized.startsWith(
        project.cacheDir
      ),
      s"»$target« is not within »$project«"
    )
    new ProjectPath(normalized)(project)

case class Project private (root: Path, adhoc: Option[Path], config: ProjectConfig):

  val outputdir: Path = root `resolve` config.output
  val cacheDir: Path = config.cache match
    case None    => outputdir `resolve` "cache"
    case Some(p) => root `resolve` p

  val outputdirWeb: Path = outputdir `resolve` "web"
  val outputdirPdf: Path = outputdir `resolve` "pdfs"

  val pdfTemplatePath: ProjectPath =
    ProjectPath(this: @unchecked, outputdir.resolve("templates").resolve("default-template.tex.scim"))

  /** Does follow symlinks.
    * Ignores files and folders starting with a .
    */
  val projectFiles: Seq[ProjectPath] =
    val source = root
    import scala.jdk.CollectionConverters.*
    def hasDotComponent(c: Path): Boolean =
      source.relativize(c).iterator().asScala.exists { _.toString.startsWith(".") }

    Using(Files.walk(source, FileVisitOption.FOLLOW_LINKS)): stream =>
      stream.iterator().asScala.filter: (c: Path) =>
        !hasDotComponent(c) && !c.startsWith(outputdir) && !c.startsWith(cacheDir)
      .map(asProjectPath).toVector
    .get :+ pdfTemplatePath

  val projectFilenames: Map[Path, Seq[ProjectPath]] =
    projectFiles.groupBy(_.absolute.getFileName)

  def byExtension(ending: String): Seq[ProjectPath] =
    val toTest = s".$ending"
    projectFiles.filter: c =>
      Files.isRegularFile(c.absolute) &&
      c.absolute.getFileName.toString.endsWith(toTest)

  def sources = byExtension("scim")

  val bibfileDBLPcache: ProjectPath = asProjectPath(cacheDir.resolve("dblpcache.bib"))
  val bibEntryCache: ProjectPath    = asProjectPath(cacheDir.resolve("bibentries.json"))

  def bibfiles = byExtension("bib") ++ Option.when(Files.exists(bibfileDBLPcache.absolute))(bibfileDBLPcache)

  // be careful about initialization below, the following two do leak the (partially uninitialized) this reference

  val imagePaths = ImagePaths(this)

  def cachePath(target: Path): ProjectPath     = ProjectPath(this, cacheDir.resolve(target))
  def asProjectPath(target: Path): ProjectPath = ProjectPath.apply(this: @unchecked, target)

  def resolveUnchecked(currentWorkingDirectory: Path, rawPath: Path): Path =
    val res =
      if rawPath.isAbsolute then root.resolve(Path.of("/").relativize(rawPath))
      else currentWorkingDirectory `resolve` rawPath
    cli.trace(s"lookup of $rawPath in $currentWorkingDirectory was $res")
    res.normalize()

  /** Does a project global file local resolve of the given path.
    * Ensures that only files in the current project are accessed
    */
  def resolve(currentWorkingDirectory: Path, pathString: Path): Option[ProjectPath] =
    val res = resolveUnchecked(currentWorkingDirectory, pathString)
    Some(res).filter(p => p.startsWith(root) && (Files.isRegularFile(p) || Files.notExists(p))).map { p =>
      ProjectPath(this, p)
    }
  def resolve(currentWorkingDirectory: Path, pathString: String): Option[ProjectPath] =
    resolve(currentWorkingDirectory, Path.of(pathString))

object Project:
  val scitzenconfig: String = "scitzen.project"

  def apply(root: Path, adhoc: Option[Path], config: ProjectConfig) =
    val absolutelyNormal = root.toAbsolutePath.normalize()
    require(Files.isDirectory(absolutelyNormal), "project root must be a directory")
    new Project(absolutelyNormal, adhoc, config)

  def findRoot(source: Path): Option[Path] =
    if Files.isRegularFile(source `resolve` scitzenconfig) then Some(source)
    else Option(source.getParent).flatMap(findRoot)

  def fromSource(file: Path): Option[Project] =
    findRoot(file.toAbsolutePath) match
      case None =>
        val adHocRoot = if Files.isDirectory(file) then file else file.getParent
        Some(Project(adHocRoot, Some(file), ProjectConfig()))
      case Some(file) => fromConfig(file)

  def fromConfig(file: Path): Option[Project] =
    val configContent = Files.readAllBytes(file `resolve` scitzenconfig)
    val value         = ProjectConfig.parse(configContent)
    Some(Project(file, None, value))

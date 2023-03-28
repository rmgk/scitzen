package scitzen.generic

import java.nio.file.{Files, Path}
import scitzen.compat.Logging.scribe
import scitzen.cli.Format

case class HtmlPathManager(cwf: Path, project: Project, articleOutputDir: Path):

  val cwd = if Files.isDirectory(cwf) then cwf else cwf.getParent

  def resolve(path: String): Option[Path] = project.resolve(cwd, path)

  def articleOutputPath(article: Article): Path =
    def genName = s"${article.date.map(_.full).getOrElse("")} ${article.title}"
    val name    = article.filename.getOrElse(genName)
    articleOutputDir resolve Format.sluggify(s"$name.html")

  def relativizeToProject(target: Path): Path = project.relativizeToProject(target)

  def relativizeImage(targetFile: Path): Path =
    def translateImage(image: Path): Path =
      (articleOutputDir resolve "images").resolve(project.root.relativize(image))
    articleOutputDir.relativize(translateImage(targetFile))

  def relativeArticleTarget(targetPost: Article): Path =
    articleOutputDir.relativize(articleOutputPath(targetPost))

  def changeWorkingFile(parent: Path): HtmlPathManager = copy(cwf = parent)

  def copyResources(resources: Iterable[(Path, Path)]) =
    resources.foreach {
      case (img, path) =>
        val target = articleOutputDir.resolve(path)
        if !Files.exists(target) then
          scribe.info(s"hardlink $img to $target")
          Files.createDirectories(target.getParent)
          Files.createLink(target, img)
    }

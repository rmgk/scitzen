package scitzen.generic

import java.nio.file.{Files, Path}
import scitzen.compat.Logging.scribe
import scitzen.cli.Format
import scitzen.sast.Section

class HtmlPathManager(project: Project):

  val articleOutputDir: Path = project.outputdirWeb

  // def resolve(path: String): Option[Path] = project.resolve(cwd, path)

  def articleOutputPath(article: Section): Path =
    def genName = s"${article.date.map(_.full).getOrElse("")} ${article}"
    val name    = article.filename.getOrElse(genName)
    articleOutputDir resolve Format.sluggify(s"$name.html")

  //def relativizeToProject(target: Path): ProjectPath = project.asProjectPath(target)

  def relativizeImage(targetFile: ProjectPath): Path =
    def translateImage(image: ProjectPath): Path =
      (articleOutputDir resolve "images").resolve(image.relative)
    articleOutputDir.relativize(translateImage(targetFile))

  def relativeArticleTarget(targetPost: Section): Path =
    articleOutputDir.relativize(articleOutputPath(targetPost))

  def copyResources(resources: Iterable[(ProjectPath, Path)]) =
    resources.foreach {
      case (img, path) =>
        val target = articleOutputDir.resolve(path)
        if !Files.exists(target) then
          scribe.info(s"hardlink $img to $target")
          Files.createDirectories(target.getParent)
          Files.createLink(target, img.absolute)
          ()
    }

package scitzen.generic

import java.nio.file.{Path, Paths}

import better.files._
import scitzen.cli.Format

case class HtmlPathManager(cwf: File, project: Project, articleOutputDir: File) {

  val cwd = if (cwf.isDirectory) cwf else cwf.parent

  def resolve(path: String) = project.resolve(cwd, path)

  def articleOutputPath(article: Article): File = {
    articleOutputDir / Format.sluggify(s"${article.date.map(_.full).getOrElse("")} ${article.title}.html")
  }
  def translateImage(image: File): File = {
    (project.outputdir / "images").path.resolve(project.root.relativize(image))
  }

  def relativizeToProject(target: File): Path = {
    Paths.get("/").resolve(project.root.relativize(target))
  }

  def relativizeImage(targetFile: File): Path = {
    articleOutputDir.relativize(translateImage(targetFile))
  }

  def relativeArticleTarget(targetPost: Article): Path = {
    articleOutputDir.relativize(articleOutputPath(targetPost))
  }

  def findDoc(pathString: String): Option[Document] =
    project.findDoc(cwd, pathString)

  def changeWorkingFile(parent: File): HtmlPathManager = copy(cwf = parent)

  def copyResources(resources: Iterable[(File, Path)]) =
    resources.foreach {
      case (img, path) =>
        val target = File(articleOutputDir.path.resolve(path))
        if (!target.exists) {
          scribe.info(s"hardlink $img to $target")
          target.parent.createDirectoryIfNotExists()
          img.linkTo(target, symbolic = false)
        }
    }

}

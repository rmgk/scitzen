package scitzen.generic

import java.nio.file.{Files, Path}
import scitzen.compat.Logging.cli
import scitzen.sast.Section

class HtmlPathManager(project: Project):

  val articleOutputDir: Path = project.outputdirWeb

  def articleOutputPath(article: Section): Path =
    articleOutputDir resolve article.relativePath

  def relativizeImage(targetFile: ProjectPath): Path =
    Path.of("resources").resolve(targetFile.relativeToProject)

  def relativeArticleTarget(targetPost: Section): Path =
    articleOutputDir.relativize(articleOutputPath(targetPost))

  def copyResources(resources: Iterable[(ProjectPath, Path)]): Unit =
    resources.foreach {
      case (img, path) =>
        val target = articleOutputDir.resolve(path)
        if !Files.exists(img.absolute) then
          cli.warn(s"source image missing: ${img.absolute}")
          ()
        else if !Files.exists(target) then
          cli.trace(s"hardlink $img to $target")
          Files.createDirectories(target.getParent)
          Files.createLink(target, img.absolute)
          ()
    }

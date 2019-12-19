package scitzen.generic

import java.nio.file.Path

import better.files._

case class HtmlPathManager(val cwf: File, project: Project, outputDir: File) {

  val cwd = if (cwf.isDirectory) cwf else cwf.parent

  val currentTargetDir = translatePost(cwf).parent

  def translatePost(post: File): File = {
    if (post.isDirectory) project.outputdir / "index.html"
    else outputDir / post.name.toString.replace(".scim", ".html")
  }
  def translateImage(image: File): File = {
    (project.outputdir / "images").path.resolve(project.root.relativize(image))
  }


  def relativizeImage(targetFile: File): Path = {
    currentTargetDir.relativize(translateImage(targetFile))
  }

  def relativizePost(targetPost: File): Path = {
    currentTargetDir.relativize(translatePost(targetPost))
  }

  def findDoc(pathString: String): Option[ParsedDocument] =
    project.findDoc(cwd, pathString)

  def changeWorkingFile(parent: File): HtmlPathManager = copy(cwf = parent)

  def copyResources(resources: Map[File, Path]) =
    resources.foreach { case (img, path) =>
      val target = File(project.outputdir.path.resolve(path))
      scribe.info(s"copy $img to $target")
      target.parent.createDirectoryIfNotExists()
      img.copyTo(target, overwrite = true)
    }

}

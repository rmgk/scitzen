package scitzen.generic

import java.nio.file.Path

import better.files._

case class HtmlPathManager(val cwf: File, project: Project, outputDir: File) {

  val cwd = if(cwf.isDirectory) cwf else cwf.parent

  def translatePost(post: File): File = {
    outputDir / post.name.toString.replace(".scim", ".html")
  }
  def translateImage(image: File): File = {
    (project.outputdir / "images").path.resolve(project.root.relativize(image))
  }


  def relativizeImage(targetFile: File): Path = {
    outputDir.relativize(translateImage(targetFile))
  }

  def relativizePost(targetPost: File): Path = {
    if (cwf.isSamePathAs(project.root)) project.outputdir.relativize(translatePost(targetPost))
    else outputDir.relativize(translatePost(targetPost))
  }

  def findDoc(pathString: String): Option[ParsedDocument] =
    project.findDoc(cwd, pathString)

  def changeWorkingFile(parent: File): HtmlPathManager = copy(cwf = parent)

  def copyImages(images: List[File]) = images.foreach { img =>

    val target = translateImage(img)
    scribe.info(s"copy $img to $target")
    target.parent.createDirectoryIfNotExists()
    img.copyTo(target, overwrite = true)
  }

}

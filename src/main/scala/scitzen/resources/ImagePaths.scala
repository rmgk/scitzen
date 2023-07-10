package scitzen.resources

import scitzen.generic.{Project, ProjectPath}
import scitzen.sast.Section

import java.nio.file.Path

class ImagePaths(project: Project):
  val html   = ConversionDispatch(project, ImageTarget.Html)
  val tex    = ConversionDispatch(project, ImageTarget.Tex)
  val raster = ConversionDispatch(project, ImageTarget.Raster)
  def lookup(imageTarget: ImageTarget): ConversionDispatch = imageTarget match
    case ImageTarget.Html   => html
    case ImageTarget.Tex    => tex
    case ImageTarget.Raster => raster

  /** Produce relative path that can be included in the output document to refer to the target file */
  def relativizeImage(targetFile: ProjectPath): Path =
    Path.of("resources").resolve(
      project.root.relativize(targetFile.absolute)
    )

  // not sure if here is the best place for these, but good enough
  val articleOutputDir: Path = project.outputdirWeb
  def articleOutputPath(article: Section): Path =
    articleOutputDir resolve article.relativePath
  def relativeArticleTarget(targetPost: Section): Path =
    articleOutputDir.relativize(articleOutputPath(targetPost))

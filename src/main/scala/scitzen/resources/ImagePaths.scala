package scitzen.resources

import scitzen.project.{Project, ProjectPath}

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

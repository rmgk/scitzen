package scitzen.images

import scitzen.generic.Project

class ImagePaths(project: Project):
  val html   = ConversionDispatch(project, ImageTarget.Html)
  val tex    = ConversionDispatch(project, ImageTarget.Tex)
  val raster = ConversionDispatch(project, ImageTarget.Raster)
  def lookup(imageTarget: ImageTarget): ConversionDispatch = imageTarget match
    case ImageTarget.Html   => html
    case ImageTarget.Tex    => tex
    case ImageTarget.Raster => raster

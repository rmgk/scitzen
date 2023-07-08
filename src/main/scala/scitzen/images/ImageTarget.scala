package scitzen.images

import scitzen.generic.ProjectPath

enum ImageTarget(
    val name: String,
    val preferredFormat: String,
    val alternative: List[String],
    val unsupportedFormat: List[String]
):
  def requiresConversion(filename: ProjectPath): Boolean =
    unsupportedFormat.exists(fmt => filename.absolute.toString.endsWith(fmt))
  def choices: List[Filetype] = (preferredFormat :: alternative).flatMap(Filetype.lookup.get)
  case Html   extends ImageTarget("html target", "svg", Nil, List("pdf", "tex"))
  case Tex    extends ImageTarget("tex target", "pdf", List("jpg"), List("svg", "tex", "webp"))
  case Raster extends ImageTarget("raster target", "png", Nil, List("svg", "pdf", "tex"))

package scitzen.resources

import scitzen.project.ProjectPath
import scitzen.resources.Filetype.*

enum ImageTarget(
    val name: String,
    val choices: List[Filetype],
    val unsupportedFormat: Set[Filetype]
):
  def requiresConversion(filename: ProjectPath): Boolean =
    unsupportedFormat.exists(fmt => filename.absolute.toString.endsWith(s".${fmt.extension}"))
  case Html   extends ImageTarget("html target", List(svg), Set(pdf))
  case Tex    extends ImageTarget("tex target", List(pdf, jpg), Filetype.all -- List(png, pdf, jpg))
  case Raster extends ImageTarget("raster target", List(png), Filetype.all -- List(png, jpg))

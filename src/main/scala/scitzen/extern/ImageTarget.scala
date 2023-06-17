package scitzen.extern

import scitzen.extern.ImageTarget.*
import scitzen.generic.Project
import scitzen.sast.Attributes

import java.nio.file.Path

enum ImageTarget(val name: String, val preferredFormat: String, val unsupportedFormat: List[String]):
  def requiresConversion(filename: String): Boolean = unsupportedFormat.exists(fmt => filename.endsWith(fmt))
  case Undetermined extends ImageTarget("undetermined target", "", Nil)
  case Html         extends ImageTarget("html target", "svg", List("pdf", "tex"))
  case Tex          extends ImageTarget("tex target", "pdf", List("svg", "tex"))
  case Raster       extends ImageTarget("raster target", "png", List("svg", "pdf", "tex"))

case class ITargetPrediction(project: Project, cwd: Path):
  def predictMacro(attributes: Attributes): Attributes =
    List(Html, Tex, Raster).foldLeft(attributes) { case (attr, target) =>
      if target.requiresConversion(attr.target) then
        val abs =
          project.resolve(cwd, attr.target).getOrElse(throw IllegalStateException(s"could not resolve ${attr.target}"))
        val filename   = s"${ImageConverter.nameWithoutExtension(abs.relative)}.${target.preferredFormat}"
        val rel        = abs.relative
        val prediction = project.cacheDir resolve "convertedImages" resolve rel.toString resolve filename
        attr.updated(s"${target.name}", project.asProjectPath(prediction).toString)
      else attr
    }

  def predictBlock(attributes: Attributes): Attributes =
    List(Html, Tex, Raster).foldLeft(attributes) { case (attr, target) =>
      val hash       = attributes.named("content hash")
      val prediction = project.cacheDir resolve hash resolve s"$hash.${target.preferredFormat}"
      attr.updated(s"${target.name}", project.asProjectPath(prediction).toString)
    }

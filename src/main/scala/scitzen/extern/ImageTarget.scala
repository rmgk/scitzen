package scitzen.extern

import scitzen.generic.{DocumentDirectory, PreprocessedResults, Project}
import scitzen.outputs.{Includes, SastToTextConverter}
import scitzen.parser.Parse
import scitzen.sast.{Attributes, Block, Fenced, Prov}

import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime
import scala.jdk.CollectionConverters.*
import scitzen.extern.ImageTarget.*

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
        val filename   = s"${ImageConverter.nameWithoutExtension(abs)}.${target.preferredFormat}"
        val rel        = project.root.relativize(abs)
        val prediction = project.cacheDir resolve "convertedImages" resolve rel.toString resolve filename
        attr.updated(s"${target.name}", project.relativizeToProject(prediction).toString)
      else attr
    }

  def predictBlock(attributes: Attributes): Attributes =
    List(Html, Tex, Raster).foldLeft(attributes) { case (attr, target) =>
      val hash       = attributes.named("content hash")
      val prediction = project.cacheDir resolve hash resolve s"$hash.${target.preferredFormat}"
      attr.updated(s"${target.name}", project.relativizeToProject(prediction).toString)
    }

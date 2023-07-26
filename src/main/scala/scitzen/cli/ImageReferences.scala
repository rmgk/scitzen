package scitzen.cli

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scitzen.contexts.{FileDependency, TargetedFileDependency}
import scitzen.project.{References, TitledArticle}
import scitzen.resources.ImageTarget
import scitzen.sast.{DCommand, Directive}

import java.nio.file.{Files, Path}

object ImageReferences:

  case class Reference(file: String, start: Int, end: Int)
  implicit val rferenceRW: JsonValueCodec[Map[String, List[Reference]]] = JsonCodecMaker.make

  def listAll(anal: ConversionAnalysis, output: Path, selected: List[TitledArticle]): List[TargetedFileDependency] =
    val fileImageMap: Map[String, List[(Reference, TargetedFileDependency)]] = selected.map { titled =>

      def art = titled.article

      val blockImageDirectives: List[Directive] = art.context.convertBlocks.flatMap: block =>
        anal.block.substitute(block).flatMap:
          case img @ Directive(DCommand.Image, _) => Some(img)
          case _                                  => None

      val images = (art.context.imageDirectives ++ blockImageDirectives).flatMap { directive =>
        References.resolveResource(anal.project, art.doc, directive.attributes.target).flatMap: orig =>
          locally:
            if ImageTarget.Raster.requiresConversion(orig)
            then
              anal.project.imagePaths.raster.predictTarget(orig)
            else Some(orig)
          .map: path =>
            (
              Reference(
                path.absolute.toString,
                art.doc.reporter.bytePosToCodepointPos(directive.prov.start),
                art.doc.reporter.bytePosToCodepointPos(directive.prov.end)
              ),
              TargetedFileDependency(FileDependency(path, orig, path.absolute), path)
            )
      }
      art.doc.path.absolute.toString -> images
    }.filter(_._2.nonEmpty).toMap
    Files.write(
      output,
      writeToArray(
        fileImageMap.view.mapValues(_.map(_._1)).toMap,
        WriterConfig.withIndentionStep(2)
      )(rferenceRW)
    )
    fileImageMap.valuesIterator.flatMap(_.map(_._2)).toList

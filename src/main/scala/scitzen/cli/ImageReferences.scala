package scitzen.cli

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scitzen.contexts.FileDependency
import scitzen.resources.ImageTarget

import java.nio.file.{Files, Path}

object ImageReferences:

  case class Reference(file: String, start: Int, end: Int)
  implicit val rferenceRW: JsonValueCodec[Map[String, List[Reference]]] = JsonCodecMaker.make

  def listAll(anal: ConversionAnalysis, output: Path): List[FileDependency] =
    val fileImageMap: Map[String, List[(Reference, FileDependency)]] = anal.directory.articles.map { art =>


      val images = art.context.imageDirectives.flatMap { directive =>
        art.doc.resolve(directive.attributes.target).flatMap: orig =>
          locally:
            if ImageTarget.Raster.requiresConversion(orig)
            then anal.project.imagePaths.raster.predictTarget(orig)
            else Some(orig)
          .map: path =>
            (
              Reference(
                path.absolute.toString,
                art.doc.reporter.bytePosToCodepointPos(directive.prov.start),
                art.doc.reporter.bytePosToCodepointPos(directive.prov.end)
              ),
              FileDependency(path, orig, path.absolute, path)
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

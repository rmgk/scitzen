package scitzen.cli

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import java.nio.file.{Files, Path}

object ImageReferences:

  case class Reference(file: String, start: Int, end: Int)
  implicit val rferenceRW: JsonValueCodec[Map[String, List[Reference]]] = JsonCodecMaker.make

  def listAll(anal: ConversionAnalysis, output: Path): Unit =
    val fileImageMap: Map[String, List[Reference]] = anal.directory.articles.map { art =>

      val images = art.context.imageDirectives.flatMap { directive =>

        val path = anal.project.imagePaths.raster.predictTarget(art.doc.resolve(directive.attributes.target).get).get
        // val (line, column) = fd.parsed.reporter.indexToPosition(mcro.attributes.prov.start)
        Some(Reference(
          path.absolute.toString,
          art.doc.reporter.bytePosToCodepointPos(directive.prov.start),
          art.doc.reporter.bytePosToCodepointPos(directive.prov.end)
        ))
      }
      art.doc.path.absolute.toString -> images
    }.filter(_._2.nonEmpty).toMap
    Files.write(
      output,
      writeToArray(
        fileImageMap,
        WriterConfig.withIndentionStep(2)
      )(rferenceRW)
    )
    ()

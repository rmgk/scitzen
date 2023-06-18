package scitzen.cli

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scitzen.extern.ImageTarget
import scitzen.compat.Logging.scribe

import java.nio.file.{Files, Path}

object ImageReferences:

  case class Reference(file: String, start: Int, end: Int)
  implicit val rferenceRW: JsonValueCodec[Map[String, List[Reference]]] = JsonCodecMaker.make

  def listAll(anal: ConversionAnalysis, output: Path): Unit =
    val fileImageMap: Map[String, List[Reference]] = anal.directory.articles.map { art =>

      val cwd = art.sourceDoc.path.directory

      val images = art.context.imageDirectives.flatMap { mcro =>
        val path = mcro.attributes.named.getOrElse(ImageTarget.Raster.name, mcro.attributes.target)
        anal.project.resolve(cwd, path) match
          case Some(target) =>
            // val (line, column) = fd.parsed.reporter.indexToPosition(mcro.attributes.prov.start)
            Some(Reference(
              target.toString,
              art.sourceDoc.reporter.bytePosToCodepointPos(mcro.prov.start),
              art.sourceDoc.reporter.bytePosToCodepointPos(mcro.prov.end)
            ))
          case None =>
            scribe.warn(s"could not find $path in $cwd")
            None
      }
      art.sourceDoc.path.absolute.toString -> images
    }.filter(_._2.nonEmpty).toMap
    Files.write(
      output,
      writeToArray(
        fileImageMap,
        WriterConfig.withIndentionStep(2)
      )(rferenceRW)
    )
    ()

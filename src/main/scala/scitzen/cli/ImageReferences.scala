package scitzen.cli

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scitzen.generic.{DocumentDirectory, Project}
import scitzen.outputs.SastToSastConverter
import scitzen.sast.DCommand
import scitzen.extern.ImageTarget
import scitzen.compat.Logging.scribe

import java.nio.file.Path

object ImageReferences:

  case class Reference(file: String, start: Int, end: Int)
  implicit val rferenceRW: JsonValueCodec[Map[String, List[Reference]]] = JsonCodecMaker.make

  def listAll(project: Project, documentDirectory: DocumentDirectory, output: Path): Unit =
    val fileImageMap: Map[String, List[Reference]] = documentDirectory.documents.map { doc =>
      val cwf = doc.file
      val cwd = cwf.parent

      val convertedCtx = new SastToSastConverter(doc, project).run()

      val images = convertedCtx.imageMacros.filter(_.command == DCommand.Image).flatMap { mcro =>
        val path = mcro.attributes.named.getOrElse(ImageTarget.Raster.name, mcro.attributes.target)
        project.resolve(cwd, path) match
          case Some(target) =>
            // val (line, column) = fd.parsed.reporter.indexToPosition(mcro.attributes.prov.start)
            Some(Reference(target.pathAsString, mcro.prov.start, mcro.prov.end))
          case None =>
            scribe.warn(s"could not find $path in $cwd")
            None
      }
      cwf.pathAsString -> images
    }.filter(_._2.nonEmpty).toMap
    better.files.File(output).writeByteArray(writeToArray(
      fileImageMap,
      WriterConfig.withIndentionStep(2)
    )(rferenceRW))

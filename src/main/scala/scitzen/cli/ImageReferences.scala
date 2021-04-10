package scitzen.cli

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scitzen.extern.ImageConverter
import scitzen.generic.{DocumentDirectory, Project}
import scitzen.outputs.SastToSastConverter
import scitzen.sast.MacroCommand

object ImageReferences {

  case class Reference(file: String, start: Int, end: Int)

  implicit val rferenceRW: JsonValueCodec[Map[String, List[Reference]]] = JsonCodecMaker.make

  def listAll(project: Project, documentDirectory: DocumentDirectory) = {
    val fileImageMap: Map[String, List[Reference]] = documentDirectory.documents.map { doc =>
      val cwf = doc.file
      val cwd = cwf.parent

      val convertedCtx = new SastToSastConverter(
        project,
        doc,
        Some(new ImageConverter(
          project,
          preferredFormat = "png",
          unsupportedFormat = List("pdf", "svg"),
          documentDirectory
        ))
      ).run()

      import scala.jdk.CollectionConverters._
      convertedCtx.tasks.asJava.parallelStream().forEach { ct =>
        ct.run()
      }

      val images = convertedCtx.partialMacros.filter(_.command == MacroCommand.Image).flatMap { mcro =>
        val path = mcro.attributes.target
        project.resolve(cwd, path) match {
          case Some(target) =>
            //val (line, column) = fd.parsed.reporter.indexToPosition(mcro.attributes.prov.start)
            Some(Reference(target.pathAsString, mcro.attributes.prov.start, mcro.attributes.prov.end))
          case None =>
            scribe.warn(s"could not find $path in $cwd")
            None
        }
      }
      cwf.pathAsString -> images
    }.filter(_._2.nonEmpty).toMap
    project.outputdir./("image-file-map.json").writeByteArray(writeToArray(
      fileImageMap,
      WriterConfig.withIndentionStep(2)
    )(rferenceRW))
  }
}

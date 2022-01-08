package scitzen.cli

import com.github.plokhotnyuk.jsoniter_scala.core._
import scitzen.generic.{DocumentDirectory, Project}
import scitzen.outputs.SastToSastConverter
import scitzen.sast.DCommand
import scitzen.compat.Codecs.Reference
import scitzen.compat.Codecs.rferenceRW

object ImageReferences:

  def listAll(project: Project, documentDirectory: DocumentDirectory): Unit =
    val fileImageMap: Map[String, List[Reference]] = documentDirectory.documents.map { doc =>
      val cwf = doc.file
      val cwd = cwf.parent

      val convertedCtx = new SastToSastConverter(doc, project).run()

      //val converter = new ImageConverter(
      //    project,
      //    preferredFormat = "png",
      //    unsupportedFormat = List("pdf", "svg"),
      //    documentDirectory
      //  )

      val images = convertedCtx.imageMacros.filter(_.command == DCommand.Image).flatMap { mcro =>
        val path = mcro.attributes.target
        project.resolve(cwd, path) match
          case Some(target) =>
            //val (line, column) = fd.parsed.reporter.indexToPosition(mcro.attributes.prov.start)
            Some(Reference(target.pathAsString, mcro.prov.start, mcro.prov.end))
          case None =>
            scribe.warn(s"could not find $path in $cwd")
            None
      }
      cwf.pathAsString -> images
    }.filter(_._2.nonEmpty).toMap
    project.outputdir./("image-file-map.json").writeByteArray(writeToArray(
      fileImageMap,
      WriterConfig.withIndentionStep(2)
    )(rferenceRW))

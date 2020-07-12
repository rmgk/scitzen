package scitzen.cli

import cats.data.Chain
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scitzen.extern.ImageConverter
import scitzen.generic.{ConversionContext, DocumentDirectory, Project}
import scitzen.outputs.SastToSastConverter
import scitzen.parser.Sast

object ImageReferences {

  case class Reference(file: String, start: Int, end: Int)

  implicit val rferenceRW: JsonValueCodec[Map[String, List[Reference]]] = JsonCodecMaker.make

  def listAll(project: Project) = {
    val fileImageMap: Map[String, List[Reference]] = DocumentDirectory(project.root).documents.map { doc =>
      val cwf = doc.file
      val cwd = cwf.parent

      val convertedCtx = new SastToSastConverter(
        project,
        doc.file,
        doc.reporter,
        new ImageConverter(project, preferredFormat = "png", unsupportedFormat = List("pdf", "svg"))
      ).convertSeq(doc.sast)(ConversionContext(Chain.empty[Sast]))

      import scala.jdk.CollectionConverters._
      convertedCtx.tasks.asJava.parallelStream().forEach { ct =>
        ct.run()
      }

      val images = convertedCtx.imageMacros.flatMap { mcro =>
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

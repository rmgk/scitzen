package scitzen.cli

import scitzen.generic.{ConversionContext, Project, Sast}
import scitzen.outputs.SastToSastConverter
import scitzen.parser.MacroCommand
import cats.data.Chain
import scitzen.extern.ImageConverter

object ImageReferences {

  case class Reference(file: String, start: Int, end: Int)

  implicit val rferenceRW: upickle.default.ReadWriter[Reference] = upickle.default.macroRW

  def listAll(project: Project) = {
    val fileImageMap = project.documentManager.fulldocs.map { fd =>
      val doc = fd.parsed
      val cwf = fd.parsed.file
      val cwd = cwf.parent

      val convertedCtx = new SastToSastConverter(
        project,
        doc.file,
        doc.reporter,
        new ImageConverter(project, preferredFormat = "png", unsupportedFormat = List("pdf", "svg"))
        ).convertSeq(fd.sast)(ConversionContext(Chain.empty[Sast]))

      import scala.jdk.CollectionConverters._
      convertedCtx.tasks.asJava.parallelStream().forEach { ct =>
        ct.run()
      }


      val analyzed = fd.analyzed.analyzer.analyze(convertedCtx.data.toList)

      val images = analyzed.macros.filter(_.command == MacroCommand.Image).flatMap { mcro =>
        val path = mcro.attributes.target
        project.resolve(cwd, path) match {
          case Some(target) =>
            //val (line, column) = fd.parsed.reporter.indexToPosition(mcro.attributes.prov.start)
            Some(Reference(target.pathAsString, mcro.attributes.prov.start, mcro.attributes.prov.end))
          case None         =>
            scribe.warn(s"could not find $path in $cwd")
            None
        }
      }
      cwf.pathAsString -> images
    }.filter(_._2.nonEmpty).toMap
    project.outputdir./("image-file-map.json").write(upickle.default.write(fileImageMap, 2))
  }
}

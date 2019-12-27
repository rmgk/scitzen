package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}

import cats.data.Chain
import scitzen.generic.{ConversionContext, ImageConverter, Project}
import scitzen.outputs.SastToSastConverter

import scala.jdk.CollectionConverters._

object ConvertGeneric {
  implicit val charset: Charset = StandardCharsets.UTF_8

  def convert(project: Project): Unit = {

    scribe.info(s"project: $project")

    val cacheDir = project.cacheDir

    cacheDir.createDirectories()

    val preConversionContext = ConversionContext(
      Chain.empty[String])

    val converted = project.documentManager.documents.map { doc =>
      new SastToSastConverter(
        project,
        doc.file.parent,
        doc.reporter,
        new ImageConverter(project, "svg")
        ).convertSeq(doc.sast)(preConversionContext)
    }

    converted.flatMap(_.tasks).asJava.parallelStream().forEach { ct =>
      ct.run()
    }

  }

}

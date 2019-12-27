package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.data.Chain
import com.monovore.decline.{Command, Opts}
import scitzen.generic.{ConversionContext, ImageConverter, Project}
import scitzen.outputs.SastToSastConverter

import scala.jdk.CollectionConverters._

object ConvertGeneric {
  implicit val charset: Charset = StandardCharsets.UTF_8
  val optSource: Opts[Path] = Opts.argument[Path](metavar = "path")


  val command: Command[Unit] = Command(name = "gen",
                                       header = "Convert Scim to Sast.") {
    optSource.map { sourcePath =>
      //val sync = syncFileRelOption.map2(syncPos)((f, p) => File(f) -> p)
      Project.fromSource(File(sourcePath)).foreach { project =>
        convert(project)
      }
    }
  }


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

package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.data.Chain
import com.monovore.decline.{Command, Opts}
import scitzen.generic.{ConversionContext, GenIndexPage, ImageConverter, PDReporter, Project}
import scitzen.outputs.SastToTexConverter

object ConvertGeneric {
  implicit val charset: Charset = StandardCharsets.UTF_8
  val optSource  : Opts[Path]         = Opts.argument[Path](metavar = "path")


  val command: Command[Unit] = Command(name = "gen",
                                       header = "Convert Scim to Sast.") {
    optSource.map { sourcePath =>
      //val sync = syncFileRelOption.map2(syncPos)((f, p) => File(f) -> p)
      Project.fromSource(File(sourcePath)).foreach { project =>
        convertToPdf(project)
      }
    }
  }



  def convertToPdf(project: Project): Unit = {

    scribe.info(s"project: $project")


    project.outputdir.createDirectories()

    val dm = project.documentManager

    val cacheDir = project.cacheDir

    cacheDir.createDirectories()

    val preConversionContext = ConversionContext(
      Chain.empty[String], converter = new ImageConverter(project))

    new SastToTexConverter(
      project,
      project.root,
      new PDReporter(dm.byPath(project.main.get).parsed)
      ).convert(
      if (project.sources.size <= 1) dm.byPath(project.main.get).parsed.sast else GenIndexPage.makeIndex(dm, project)
      )(preConversionContext)


  }

}

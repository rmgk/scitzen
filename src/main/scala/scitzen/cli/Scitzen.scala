package scitzen.cli

import java.nio.file.{Path, Paths}

import better.files._
import cats.implicits._
import com.monovore.decline.Visibility.Partial
import com.monovore.decline.{Command, CommandApp, Opts}
import scitzen.generic.Project
import scribe.Logger

object Scitzen
    extends CommandApp(
      name = "scitzen",
      header = "Static page generator",
      main = {

        import scribe.format._
        val myFormatter: Formatter = formatter"$message ($position)"
        Logger.root.clearHandlers().withHandler(
          formatter = myFormatter,
          minimumLevel = Some(scribe.Level.Info)
        ).replace()

        ConvertProject.command.options
      }
    )

object ConvertProject {

  val args = (
    Opts.argument[Path](metavar = "path")
      .withDefault(Paths.get("")),
    Opts.option[Path]("sync-file", metavar = "file", visibility = Partial, help = "file to show in output").orNone,
    Opts.option[Int](
      "sync-position",
      metavar = "integer",
      visibility = Partial,
      help = "character offset to show in output"
    ).orNone,
    Opts.flag("image-file-map", visibility = Partial, help = "character offset to show in output").orFalse,
    Opts.option[Path](long = "json", metavar = "path", help = "print single file structure as json")
      .orNone,
  )

  val command: Command[Unit] = Command(name = "gen", header = "Convert Scim to Sast.") {
    args.mapN {
      (sourcedirRel, syncFileRelOption, syncPos, imageFileMap, printJson) =>
        printJson match {
          case Some(path) => println(JsonSast.jsonFor(File(path)))
          case None =>
            Project.fromSource(File(sourcedirRel)) match {
              case None => scribe.error(s"could not find project for $sourcedirRel")
              case Some(project) =>
                scribe.info(s"$project")
                if (project.config.format.contains("content")) {
                  scribe.info(s"format contents")
                  Format.formatContents(project)
                }
                if (project.config.format.contains("filename")) {
                  scribe.info(s"format filenames")
                  Format.formatRename(project)
                }
                if (project.config.outputType.contains("html")) {
                  val sync = syncFileRelOption.map2(syncPos)((f, p) => File(f) -> p)
                  ConvertHtml.convertToHtml(project, sync)
                }
                if (project.config.outputType.contains("reveal")) {
                  val sync = syncFileRelOption.map2(syncPos)((f, p) => File(f) -> p)
                  ConvertRevealPresentation.convertToHtml(project, sync)
                }
                if (project.config.outputType.contains("pdf")) {
                  ConvertPdf.convertToPdf(project)
                }
                if (imageFileMap) {
                  ImageReferences.listAll(project)
                }
            }
        }
    }
  }
}

package scitzen.cli

import java.nio.file.{Path, Paths}

import better.files._
import cats.implicits._
import com.monovore.decline.Visibility.Partial
import com.monovore.decline.{Command, CommandApp, Opts}
import scitzen.generic.Project
import scribe.Logger


object Scitzen extends CommandApp(
  name = "scitzen",
  header = "Static page generator",
  main = {

    import scribe.format._
    val myFormatter: Formatter = formatter"$message ($position)"
    Logger.root.clearHandlers().withHandler(formatter = myFormatter,
                                            minimumLevel = Some(scribe.Level.Info)).replace()

    ConvertProject.command.options
  }
  )


object ConvertProject {
  val optSource: Opts[Path] = Opts.argument[Path](metavar = "path")
                                  .withDefault(Paths.get(""))


  val optSyncFile: Opts[Option[Path]] = Opts.option[Path]("sync-file", metavar = "file",
                                                          visibility = Partial,
                                                          help = "file to show in output").orNone
  val optSyncPos : Opts[Option[Int]]  = Opts.option[Int]("sync-position", metavar = "integer",
                                                         visibility = Partial,
                                                         help = "character offset to show in output").orNone

  val optImageFileMap: Opts[Boolean] = Opts.flag("image-file-map",
                                                     visibility = Partial,
                                                     help = "character offset to show in output").orFalse


  val command: Command[Unit] = Command(name = "gen",
                                       header = "Convert Scim to Sast.") {
    (optSource, optSyncFile, optSyncPos, optImageFileMap).mapN {
      (sourcedirRel, syncFileRelOption, syncPos, imageFileMap) =>
        //val sync = syncFileRelOption.map2(syncPos)((f, p) => File(f) -> p)
        Project.fromSource(File(sourcedirRel)).foreach { project =>
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

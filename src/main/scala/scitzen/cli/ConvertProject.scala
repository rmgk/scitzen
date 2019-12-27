package scitzen.cli


import java.nio.file.Path

import better.files._
import cats.implicits._
import com.monovore.decline.Visibility.Partial
import com.monovore.decline.{Command, Opts}
import scitzen.generic.Project

object ConvertProject {
  val optSource: Opts[Path] = Opts.argument[Path](metavar = "path")

  val optSyncFile: Opts[Option[Path]] = Opts.option[Path]("sync-file", metavar = "file",
                                                          visibility = Partial,
                                                          help = "file to show in output").orNone
  val optSyncPos : Opts[Option[Int]]  = Opts.option[Int]("sync-position", metavar = "integer",
                                                         visibility = Partial,
                                                         help = "character offset to show in output").orNone


  val command: Command[Unit] = Command(name = "gen",
                                       header = "Convert Scim to Sast.") {
    (optSource, optSyncFile, optSyncPos).mapN {
      (sourcedirRel, syncFileRelOption, syncPos) =>
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
      }
    }
  }
}

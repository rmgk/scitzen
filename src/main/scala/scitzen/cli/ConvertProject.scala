package scitzen.cli


import java.nio.file.Path

import better.files._
import com.monovore.decline.{Command, Opts}
import scitzen.generic.Project


object ConvertProject {
  val optSource: Opts[Path] = Opts.argument[Path](metavar = "path")


  val command: Command[Unit] = Command(name = "gen",
                                       header = "Convert Scim to Sast.") {
    optSource.map { sourcePath =>
      //val sync = syncFileRelOption.map2(syncPos)((f, p) => File(f) -> p)
      Project.fromSource(File(sourcePath)).foreach { project =>
        scribe.info(s"$project")
        if (project.config.format.contains("content")) {
          scribe.info(s"format contents")
          Format.formatContents(project)
        }
        if (project.config.format.contains("filename")) {
          scribe.info(s"format filenames")
          Format.formatRename(project)
        }
      }
    }
  }
}

package scitzen.cli

import de.rmgk.options.*
import scitzen.cli.ConvertProject.executeConversions
import scitzen.compat.Logging.scribe
import scitzen.generic.Project
import scopt.OParser

import java.nio.file.{Files, Path}

object ScitzenCommandline {

  def main(args: Array[String]): Unit = {
    val optInstance = ClOptions()
    OParser.parse(
      scopt.OParser.sequence(
        scopt.OParser.builder[ClOptions].help('h', "help").hidden(),
        makeParser(optInstance, _.programName("scitzen"))
      ),
      args,
      optInstance
    ) match {
      case None =>
      case Some(options) =>
        Project.fromSource(options.path.value) match
          case None => scribe.error(s"could not find project for $options.path")
          case Some(project) =>
            executeConversions(options.sync.value, options.`image-file-map`.value, project)
    }
  }

  case class ClSync(path: Path = null, position: Int = -1)
  given scopt.Read[ClSync] = summon[scopt.Read[(Path, Int)]].map(ClSync.apply)
  case class ClOptions(
      path: Argument[Path, Single, Style.Positional] =
        Argument(
          _.text("path to project, file, or scope to compile").validate { p =>
            if Files.exists(p) then Right(()) else Left(s"»$p« does not exist")
          },
          Some(Path.of("").toAbsolutePath)
        ),
      `image-file-map`: Argument[Path, Option, Style.Named] =
        Argument(_.valueName("path").text("produce json description of generated images")),
      sync: Argument[ClSync, Option, Style.Named] =
        Argument(_.keyName("path").valueName("pos").text("sync position")),
  )
}

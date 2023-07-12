package scitzen.cli

import de.rmgk.options.*
import scitzen.cli.ConvertProject.executeConversions
import scitzen.compat.Logging.{cli, given}
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
        def run() =
          val absolute = options.path.value.map(_.toAbsolutePath.normalize)
          Project.fromSource(absolute.head) match
            case None => cli.warn(s"could not find project for", options.path.value)
            case Some(project) =>
              executeConversions(options.sync.value, options.`image-file-map`.value, project, absolute)

        if options.benchmark.value > 0
        then (0 to options.benchmark.value).foreach(_ => run())
        else run()
    }
  }

  case class ClSync(path: Path = null, position: Int = -1)
  given scopt.Read[ClSync] = summon[scopt.Read[(Path, Int)]].map(ClSync.apply)
  case class ClOptions(
      path: Argument[Path, List, Style.Positional] =
        Argument(
          _.text("path to project, file, or scope to compile").validate { p =>
            if Files.exists(p) then Right(()) else Left(s"»$p« does not exist")
          },
          Some(List(Path.of("").toAbsolutePath))
        ),
      `image-file-map`: Argument[Path, Option, Style.Named] =
        Argument(_.valueName("path").text("produce json description of generated images")),
      sync: Argument[ClSync, Option, Style.Named] =
        Argument(_.keyName("path").valueName("pos").text("sync position")),
      benchmark: Argument[Int, Single, Style.Named] = Argument(_.text("run conversions multiple times"), Some(0)),
      tracing: Argument[Boolean, Single, Style.Named] = Argument(_.text("enable trace logging"), Some(false)),
  )
}

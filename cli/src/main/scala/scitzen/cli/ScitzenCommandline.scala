package scitzen.cli

import better.files.*
import cats.data.Validated
import cats.implicits.*
import com.monovore.decline.Visibility.Partial
import com.monovore.decline.{Command, CommandApp, Opts}
import scitzen.cli.ConvertProject.executeConversions
import scitzen.extern.{ImageConverter, ImageTarget}
import scitzen.generic.{PreprocessedResults, Project, ProjectConfig}

import java.nio.file.{Path, Paths}

object ScitzenCommandline extends CommandApp(
      name = "scitzen",
      header = "Static page generator",
      main = ScitzenCommandline.fullOpts.map {
        case Right(jsonpath) =>
          println(JsonSast.jsonFor(
            File(jsonpath),
            Project(File(jsonpath).parent, ProjectConfig.parse("b=c"), Map.empty)
          ))
        case Left(options) =>
          Project.fromSource(options.path) match
            case None => scribe.error(s"could not find project for $options.path")
            case Some(project) =>
              executeConversions(options.sync, options.`image-file-map`, project, options.`use-cats-parse`)
      }
    ) {

  case class ClSync(path: File, position: Int)
  case class ClOptions(path: File, `image-file-map`: Boolean, `use-cats-parse`: Boolean, sync: Option[ClSync])

  val syncOpts: Opts[Option[ClSync]] = (
    Opts.option[Path]("sync-file", metavar = "file", visibility = Partial, help = "file to show in output").orNone,
    Opts.option[Int](
      "sync-position",
      metavar = "integer",
      help = "character offset to show in output",
      visibility = Partial,
    ).orNone
  ).tupled.mapValidated {
    case (Some(f), Some(p)) => Validated.valid(Some(ClSync(File(f), p)))
    case _                  => Validated.invalidNel("sync requires both file and position")
  }

  val mainOpts: Opts[ClOptions] = (
    Opts.argument[Path](metavar = "path").withDefault(Paths.get(""))
      .mapValidated { p =>
        val f = File(p)
        if f.exists then Validated.valid(f) else Validated.invalidNel("path must exists")
      },
    Opts.flag("image-file-map", visibility = Partial, help = "character offset to show in output").orFalse,
    Opts.flag("use-cats-parse", visibility = Partial, help = "use cats parse instead of fastparse").orFalse,
    syncOpts
  ).mapN(ClOptions.apply)

  val jsonOpts: Opts[Path] =
    Opts.option[Path](long = "json", metavar = "path", help = "print single file structure as json")

  val fullOpts: Opts[Either[ClOptions, Path]] = mainOpts.map(Either.left).orElse(jsonOpts.map(Either.right))
}

package scitzen.cli

import better.files.*
import scitzen.cli.ConvertProject.executeConversions
import scitzen.extern.{ImageConverter, ImageTarget}
import scitzen.generic.{PreprocessedResults, Project, ProjectConfig}
import scopt.OParser
import scitzen.compat.Logging.scribe
import de.rmgk.options.*

import java.awt.datatransfer.ClipboardOwner
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}

object ScitzenCommandline {

  def main(args: Array[String]): Unit = {
    val optInstance = ClOptions()
    OParser.parse(
      scopt.OParser.sequence(
        scopt.OParser.builder[ClOptions].help('h', "help").hidden(),
        makeParser("scitzen", optInstance)
      ),
      args,
      optInstance
    ) match {
      case None =>
      case Some(options) =>
        if options.json.value then
          val jsonpath = options.path.value
          println(JsonSast.jsonFor(
            jsonpath,
            Project(File(jsonpath).parent, ProjectConfig.parse("b=c".getBytes(StandardCharsets.UTF_8)), Map.empty)
          ))
        else
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
        Argument(_.text("path to project, file, or scope to compile"), Some(Paths.get(""))),
      `image-file-map`: Argument[Unit, Flag, Style.Named] =
        Argument(_.text("produce json description of generated images")),
      sync: Argument[ClSync, Option, Style.Named] =
        Argument(_.keyName("path").valueName("pos").text("sync position")),
      json: Argument[Unit, Flag, Style.Named] =
        Argument(_.text("create json from a single file"))
  )
}

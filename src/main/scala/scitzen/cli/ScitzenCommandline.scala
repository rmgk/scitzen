package scitzen.cli

import better.files.*
import com.monovore.decline.Visibility.Partial
import com.monovore.decline.{Command, CommandApp, Opts}
import scitzen.cli.ConvertProject.executeConversions
import scitzen.extern.{ImageConverter, ImageTarget}
import scitzen.generic.{PreprocessedResults, Project, ProjectConfig}
import scopt.OParser

import java.nio.file.{Path, Paths}

object ScitzenCommandline {

  def main(args: Array[String]): Unit = {
    OParser.parse(argsParser, args, ClOptions()) match {
      case None =>
      case Some(options) =>
        if options.json then
          val jsonpath = options.path
          println(JsonSast.jsonFor(
            jsonpath,
            Project(jsonpath.parent, ProjectConfig.parse("b=c"), Map.empty)
          ))
        else
          Project.fromSource(options.path) match
            case None => scribe.error(s"could not find project for $options.path")
            case Some(project) =>
              executeConversions(options.sync, options.`image-file-map`, project)
    }
  }

  case class ClSync(path: File = null, position: Int = -1)
  case class ClOptions(
      path: File = File(""),
      `image-file-map`: Boolean = false,
      sync: Option[ClSync] = None,
      json: Boolean = false
  )

  val argsParser = {
    val builder = scopt.OParser.builder[ClOptions]
    import builder.*
    scopt.OParser.sequence(
      programName("scitzen"),
      help('h', "help").hidden(),
      arg[java.io.File]("path").required().action((p, c) => c.copy(path = p.toScala)),
      opt[Unit]("image-file-map").optional().text("produce json description of generated images")
        .action((_, c) => c.copy(`image-file-map` = true)),
      opt[(Int, java.io.File)]("sync").optional().text("sync position").hidden()
        .action((s, c) => c.copy(sync = Some(ClSync(s._2.toScala, s._1)))),
      opt[Unit]("json").optional().text("create json from a single file")
        .action((_, c) => c.copy(json = true))
    )
  }
}

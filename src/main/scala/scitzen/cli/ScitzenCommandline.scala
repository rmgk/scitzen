package scitzen.cli

import de.rmgk.logging
import de.rmgk.logging.{DefaultLogPrinter, Level, Logger}
import de.rmgk.options.*
import scitzen.cli.ConvertProject.executeConversions
import scitzen.compat.Logging
import scitzen.compat.Logging.{cli, given}
import scitzen.project.Project
import scitzen.sast.Fusion

import java.nio.file.{Files, Path}

given [T](using avp: ArgumentValueParser[T]): ArgumentValueParser[List[T]] with
  override def apply(args: List[String]): (Option[List[T]], List[String]) =
    def rec(remaining: List[String], acc: List[T]): (Option[List[T]], List[String]) =
      remaining match
        case head :: _ if !head.startsWith("--") =>
          val (value, rest) = avp.apply(remaining)
          value match
            case None    => (Some(acc.reverse), remaining)
            case Some(v) => rec(rest, v :: acc)
        case other => (Some(acc), remaining)

    rec(args, Nil)

  override def valueDescription: String = s"${avp.valueDescription}*"

object ScitzenCommandline {
  def main(args: Array[String]): Unit = {
    parseArguments(args.toList):
      val benchmark = named[Int]("--benchmark", "run conversions multiple times", 0).value

      val imageMap = named[Option[Path]](
        "--image-file-map",
        "produce json description of generated images",
        None
      ).value

      val absolute: List[Path] =
        positional("scope", "path to project, file, or scope to compile", List.empty[Path]).value.map: p =>
          val normalized = p.toAbsolutePath.normalize
          if !Files.exists(normalized) then cli.warn(s"root does not exist: $normalized")
          normalized

      val projectSearchPath = absolute.headOption.getOrElse(Path.of(""))

      named[Option[String]]("--verbose", "enable trace logging", None).value match
        case Some(str) =>
          val rx = str.r
          Logging.cli = Logger(
            Level.Trace,
            new DefaultLogPrinter(printPosition = true, verboseInfo = false) {
              override def print[T](logLine: logging.LogLine[T]): Unit =
                if logLine.level != Level.Trace || rx.findFirstIn(logLine.context.enclosing.value).isDefined
                then println(format(logLine))
            }
          )
        case other =>

      val fusion = named[Boolean]("fusion", "use fusion parser", false).value

      def run(): Unit =
        Project.fromSource(projectSearchPath) match
          case None => cli.warn(s"could not find project for", projectSearchPath)
          case Some(project) =>
            if fusion
            then
              Fusion.run(project, absolute)
            else
              executeConversions(
                imageMap,
                project,
                absolute,
                named[Boolean]("--format-filenames", "adapt filenames to reflect article headers", false).value
              )

      if benchmark > 0
      then Range(0, benchmark).foreach(_ => run())
      else run()
    .printHelp()
  }
}

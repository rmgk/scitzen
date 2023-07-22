package scitzen.cli

import de.rmgk.options.*
import scitzen.cli.ConvertProject.executeConversions
import scitzen.compat.Logging.{cli, given}
import scitzen.project.Project

import java.nio.file.{Files, Path}

given [T](using avp: ArgumentValueParser[T]): ArgumentValueParser[List[T]] with
  override def apply(args: List[String]): (Option[List[T]], List[String]) =
    def rec(remaining: List[String], acc: List[T]): (Option[List[T]], List[String]) =
      if remaining.isEmpty
      then (Some(acc), Nil)
      else
        val (value, rest) = avp.apply(remaining)
        value match
          case None    => (Some(acc.reverse), remaining)
          case Some(v) => rec(rest, v :: acc)
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

      def run(): Unit =
        Project.fromSource(projectSearchPath) match
          case None => cli.warn(s"could not find project for", projectSearchPath)
          case Some(project) =>
            executeConversions(
              imageMap,
              project,
              absolute
            )

      if benchmark > 0
      then Range(0, benchmark).foreach(_ => run())
      else run()
    .printHelp()
  }
}

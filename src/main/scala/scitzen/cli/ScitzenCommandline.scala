package scitzen.cli

import de.rmgk.options.*
import de.rmgk.options.ParsedArguments.ParseResult
import scitzen.cli.ConvertProject.executeConversions
import scitzen.compat.Logging.{cli, given}
import scitzen.project.Project

import java.nio.file.{Files, Path}

object ScitzenCommandline {
  def main(args: Array[String]): Unit = {
    parseArguments(args.toList):
      val benchmark = Argument[Int]("benchmark", Style.Named, "run conversions multiple times", 0).value

      val imageMap = Argument[Option[Path]](
        "image-file-map",
        Style.Named,
        "produce json description of generated images",
        None
      ).value

      val absolute: List[Path] = RemainingArguments("scope", "path to project, file, or scope to compile").value.map: p =>
        val normalized = Path.of(p).toAbsolutePath.normalize
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

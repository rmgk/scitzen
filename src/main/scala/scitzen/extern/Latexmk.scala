package scitzen.extern

import scitzen.compat.Logging.cli
import de.rmgk.script.*
import de.rmgk.delay.*

import java.nio.file.{Files, Path}

object Latexmk:

  def latexmk(outputdir: Path, jobname: String, sourceFile: Path): Async[Any, Option[Path]] = Async:
    val start = System.nanoTime()
    cli.trace(s"compiling $sourceFile")
    Files.createDirectories(outputdir)
    val errorFile = outputdir.resolve("latexmk.err")
    val process =
      process"""tectonic
        --keep-intermediates
        --outdir
        ${outputdir.toAbsolutePath}
        ${sourceFile.toAbsolutePath}
        """
        .directory(outputdir.toFile)
        .redirectOutput(outputdir.resolve("latexmk.out").toFile)
        .redirectError(errorFile.toFile)
        .start().onExit().toAsync.bind
    if process.exitValue() == 0 then
      cli.trace(s"tex compilation of »$sourceFile« finished in ${(System.nanoTime() - start) / 1000000}ms")
      Some(outputdir.resolve(jobname + ".pdf"))
    else
      cli.warn(s"error tex compiling »$sourceFile« see »$errorFile«")
      None

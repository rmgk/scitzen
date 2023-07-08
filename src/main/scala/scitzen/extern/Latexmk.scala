package scitzen.extern

import scitzen.compat.Logging.cli

import java.nio.file.{Files, Path}

object Latexmk:

  def latexmk(outputdir: Path, jobname: String, sourceFile: Path): Option[Path] =
    val start = System.nanoTime()
    cli.trace(s"compiling $sourceFile")
    Files.createDirectories(outputdir)
    val errorFile = outputdir.resolve("latexmk.err")
    val returnCode =
      new ProcessBuilder(
        "tectonic",
        "--keep-intermediates",
        "--outdir",
        outputdir.toAbsolutePath.toString(),
        sourceFile.toAbsolutePath.toString()
      )
        // new ProcessBuilder(
        //  "latexmk",
        //  "-cd",
        //  "-halt-on-error",
        //  "-xelatex",
        //  "-interaction=nonstopmode",
        //  //"-synctex=1",
        //  "--output-directory=" + outputdir,
        //  "--jobname=" + jobname,
        //  sourceFile.pathAsString
        // )
        .directory(outputdir.toFile)
        .redirectOutput(outputdir.resolve("latexmk.out").toFile)
        .redirectError(errorFile.toFile)
        .start().waitFor()
    if returnCode == 0 then
      cli.trace(s"tex compilation of »$sourceFile« finished in ${(System.nanoTime() - start) / 1000000}ms")
      Some(outputdir.resolve(jobname + ".pdf"))
    else
      cli.warn(s"error tex compiling »$sourceFile« see »$errorFile«")
      None

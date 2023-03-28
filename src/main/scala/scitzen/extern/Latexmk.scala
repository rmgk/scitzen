package scitzen.extern

import scitzen.compat.Logging.scribe

import java.nio.file.{Files, Path}

object Latexmk:

  def latexmk(outputdir: Path, jobname: String, sourceFile: Path): Option[Path] =
    val start = System.nanoTime()
    scribe.info(s"compiling $sourceFile")
    Files.createDirectories(outputdir)
    val errorFile = outputdir.resolve("latexmk.err")
    val returnCode =
      new ProcessBuilder(
        "tectonic",
        "--keep-intermediates",
        "--outdir",
        outputdir.toString(),
        sourceFile.toString
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
      scribe.info(s"tex compilation of »$sourceFile« finished in ${(System.nanoTime() - start) / 1000000}ms")
      Some(outputdir.resolve(jobname + ".pdf"))
    else
      scribe.error(s"error tex compiling »$sourceFile« see »$errorFile«")
      None

package scitzen.extern

import better.files.File
import scitzen.compat.Logging.scribe

object Latexmk:

  def latexmk(outputdir: File, jobname: String, sourceFile: File): Option[File] =
    val start = System.nanoTime()
    scribe.info(s"compiling $sourceFile")
    outputdir.createDirectories()
    val errorFile = (outputdir / "latexmk.err")
    val returnCode =
      new ProcessBuilder(
        "tectonic",
        "--keep-intermediates",
        "--outdir",
        outputdir.toString(),
        sourceFile.pathAsString
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
        .directory(outputdir.toJava)
        .redirectOutput((outputdir / "latexmk.out").toJava)
        .redirectError(errorFile.toJava)
        .start().waitFor()
    if returnCode == 0 then
      scribe.info(s"tex compilation of »$sourceFile« finished in ${(System.nanoTime() - start) / 1000000}ms")
      Some(outputdir / (jobname + ".pdf"))
    else
      scribe.error(s"error tex compiling »$sourceFile« see »$errorFile«")
      None

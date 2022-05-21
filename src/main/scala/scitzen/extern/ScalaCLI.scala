package scitzen.extern

import better.files.File
import scitzen.compat.Logging.scribe

object ScalaCLI:

  def compile(mainOut: File, source: String): Option[String] =
    val start      = System.nanoTime()
    val hash       = Hashes.sha1hex(source)
    val outputdir = mainOut / "scala-cli" / hash
    val sourceFile = outputdir / s"source.scala"
    scribe.info(s"compiling $sourceFile")
    outputdir.createDirectories()
    sourceFile.write(source)
    val errorFile = (outputdir / "log.txt")
    val returnCode =
      new ProcessBuilder(
        "scala-cli",
        "package",
        "--force",
        "--output",
        (outputdir / "out.js").toString(),
        sourceFile.toString,
      ).directory(outputdir.toJava)
        .redirectOutput(errorFile.toJava)
        .redirectError(errorFile.toJava)
        .start().waitFor()
    if returnCode == 0 then
      scribe.info(s"scala compilation of »$sourceFile« finished in ${(System.nanoTime() - start) / 1000000}ms")
      Some((outputdir / "out.js").contentAsString)
    else
      scribe.error(s"error scala compiling »$sourceFile« see »$errorFile«")
      None

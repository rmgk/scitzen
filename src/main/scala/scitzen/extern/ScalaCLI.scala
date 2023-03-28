package scitzen.extern

import scitzen.compat.Logging.scribe

import java.nio.file.{Files, Path}

object ScalaCLI:

  def compile(mainOut: Path, source: String): Option[String] =
    val start      = System.nanoTime()
    val hash       = Hashes.sha1hex(source)
    val outputdir  = mainOut resolve "scala-cli" resolve hash
    val sourceFile = outputdir resolve s"source.scala"
    scribe.info(s"compiling $sourceFile")
    Files.createDirectories(outputdir)
    Files.writeString(sourceFile, source)
    val errorFile = outputdir resolve "log.txt"
    val returnCode =
      new ProcessBuilder(
        "scala-cli",
        "package",
        "--force",
        "--output",
        (outputdir resolve "out.js").toString(),
        sourceFile.toString,
      ).directory(outputdir.toFile)
        .redirectOutput(errorFile.toFile)
        .redirectError(errorFile.toFile)
        .start().waitFor()
    if returnCode == 0 then
      scribe.info(s"scala compilation of »$sourceFile« finished in ${(System.nanoTime() - start) / 1000000}ms")
      Some(Files.readString(outputdir resolve "out.js"))
    else
      scribe.error(s"error scala compiling »$sourceFile« see »$errorFile«")
      None

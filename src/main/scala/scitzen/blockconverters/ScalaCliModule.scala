package scitzen.blockconverters

import scitzen.compat.Logging.cli
import scitzen.extern.Hashes
import scitzen.sast.{Attributes, DCommand, Directive, Sast}

import java.nio.file.{Files, Path}

object ScalaCliModule extends BlockConverterModule {

  override def handles: String = "scalaCli"
  def convert(converterParams: ConverterParams): List[Sast] = {
    import converterParams.*
    val start      = System.nanoTime()
    val hash       = Hashes.sha1hex(content)
    val sourcepath = project.cachePath(Path.of("scala-cli").resolve(hash + ".scala")).absolute
    Files.createDirectories(sourcepath.getParent)
    Files.writeString(sourcepath, content)
    val outpath   = sourcepath resolveSibling s"$hash.js"
    val errorFile = sourcepath resolveSibling s"log-$hash.txt"
    val returnCode =
      new ProcessBuilder(
        "scala-cli",
        "--power",
        "package",
        "--force",
        "--output",
        outpath.toString,
        sourcepath.toString,
      ).directory(sourcepath.getParent.toFile)
        .redirectOutput(errorFile.toFile)
        .redirectError(errorFile.toFile)
        .start().waitFor()
    if returnCode == 0 then
      cli.info(s"scala compilation of »$sourcepath« finished in ${(System.nanoTime() - start) / 1000000}ms")
      val pp = project.asProjectPath(outpath)
      List(
        Directive(DCommand.Script, Attributes.target(pp.projectAbsolute.toString), block.meta)
      )
    else
      cli.warn(s"error scala compiling »$sourcepath« see »$errorFile«")
      Nil
  }

}

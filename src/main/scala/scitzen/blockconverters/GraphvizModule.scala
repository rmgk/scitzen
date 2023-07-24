package scitzen.blockconverters

import scitzen.compat.Logging.cli
import scitzen.extern.Hashes
import scitzen.sast.{Attributes, DCommand, Directive, Sast}

import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using

object GraphvizModule extends BlockConverterModule {

  override def handles: String = "graphviz"

  override def convert(converterParams: ConverterParams): List[Sast] =
    import converterParams.*
    val bytes  = content.getBytes(StandardCharsets.UTF_8)
    val name   = Hashes.sha1hex(bytes)
    val format = "pdf"
    val target = project.cachePath(Path.of(s"$name/$name.$format"))
    if !Files.exists(target.absolute) then
      Files.createDirectories(target.absolute.getParent)

      val layoutEngine =
        val lay = attributes.target.trim
        if lay.isEmpty then "dot"
        else lay

      val start = System.nanoTime()
      val process = new ProcessBuilder(
        "dot",
        s"-K${layoutEngine}",
        s"-T$format",
        s"-o${target.absolute.toString}",
      )
        .inheritIO().redirectInput(Redirect.PIPE).start()
      Using.resource(process.getOutputStream) { os => os.write(bytes) }
      process.waitFor()
      cli.info(s"graphviz compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    List(Directive(DCommand.Image, Attributes.target(target.projectAbsolute.toString))(block.prov))
}

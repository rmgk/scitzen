package scitzen.blockconverters

import scitzen.cli.Logging.cli
import scitzen.extern.Hashes
import scitzen.sast.{Attributes, DCommand, Directive, Sast, Attribute, attributes}

import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using

object GraphvizModule extends BlockConverterModule {

  override def handles: String = "graphviz"

  override def convert(converterParams: ConverterParams): List[Sast] =
    import converterParams.{content, project, attribute, block}
    val bytes  = content.getBytes(StandardCharsets.UTF_8)
    val name   = Hashes.sha1hex(bytes)
    val format = "pdf"
    val target = project.cachePath(Path.of(s"$name/$name.$format"))
    if !Files.exists(target.absolute) then
      Files.createDirectories(target.absolute.getParent)

      val start = System.nanoTime()
      val process = new ProcessBuilder(
        "dot",
        s"-T$format",
        s"-o${target.absolute.toString}",
      )
        .inheritIO().redirectInput(Redirect.PIPE).start()
      Using.resource(process.getOutputStream) { os => os.write(bytes) }
      process.waitFor()
      cli.info(s"graphviz compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    List(Directive(
      DCommand.Image,
      Attributes(block.attributes.all ++ Seq(
        Attribute(target.projectAbsolute.toString),
        Attribute("color", "autoinvert")
      )),
      block.meta
    ))
}

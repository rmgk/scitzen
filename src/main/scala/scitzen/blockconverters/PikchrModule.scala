package scitzen.blockconverters

import de.rmgk.script.syntax
import scitzen.compat.Logging.cli
import scitzen.extern.Hashes
import scitzen.sast.{Attribute, Attributes, DCommand, Directive, Sast}

import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using

object PikchrModule extends BlockConverterModule {

  override def handles: String = "pikchr"

  override def convert(converterParams: ConverterParams): List[Sast] =

    val bytes  = converterParams.content.getBytes(StandardCharsets.UTF_8)
    val name   = Hashes.sha1hex(bytes)
    val target = converterParams.project.cachePath(Path.of(s"$name/$name.svg"))
    if !Files.exists(target.absolute) then
      Files.createDirectories(target.absolute.getParent)
      val start = System.nanoTime()
      val pikchr = process"pikchr --svg-only -"
        .redirectError(Redirect.INHERIT)
        .redirectOutput(target.absolute.toFile)
        .start()
      Using.resource(pikchr.getOutputStream) { os => os.write(bytes) }
      pikchr.waitFor()
      cli.info(s"pikchr compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    List(Directive(
      DCommand.Image,
      Attributes(Seq(
        Attribute(target.projectAbsolute.toString),
        Attribute("css_style", "background-color:white"),
      ) ++ converterParams.block.attributes.raw)
    )(converterParams.block.prov))

}

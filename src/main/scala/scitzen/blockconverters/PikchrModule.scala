package scitzen.blockconverters

import de.rmgk.script.syntax
import scitzen.compat.Logging.cli
import scitzen.extern.Hashes
import scitzen.sast.{Attribute, Attributes, DCommand, Directive, Sast}

import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using
import scala.util.control.NonFatal

object PikchrModule extends BlockConverterModule {

  override def handles: String = "pikchr"

  val viewboxRegex = raw"""viewBox="(?<x>[\d\.]+)\s+(?<y>[\d\.]+)\s+(?<width>[\d\.]+)\s+(?<height>[\d\.]+)"""".r

  override def convert(converterParams: ConverterParams): List[Sast] =

    val bytes  = converterParams.content.getBytes(StandardCharsets.UTF_8)
    val name   = Hashes.sha1hex(bytes)
    val target = converterParams.project.cachePath(Path.of(s"$name/$name.svg"))
    if !Files.exists(target.absolute) then
      Files.createDirectories(target.absolute.getParent)
      val start = System.nanoTime()
      val pikchr = process"pikchr --svg-only -"
        .redirectError(Redirect.INHERIT)
        .start()
      Using.resource(pikchr.getOutputStream) { os => os.write(bytes) }
      pikchr.waitFor()
      val svg = Using.resource(pikchr.getInputStream) { _.readToString }
      val updated =
        try
          val res    = viewboxRegex.findFirstMatchIn(svg).get
          val width  = res.group("width").toDouble
          val height = res.group("height").toDouble
          s"""<svg width="${width}" height="$height"""" + svg.substring(4)
        catch
          case NonFatal(e) =>
            cli.warn(s"could not find viewbox during pikchr transformation", e)
            svg
      Files.writeString(target.absolute, updated, StandardCharsets.UTF_8)
      cli.info(s"pikchr compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    List(Directive(
      DCommand.Image,
      Attributes(converterParams.block.attributes.raw ++ Seq(
        Attribute(target.projectAbsolute.toString),
        Attribute("css_style", "background-color:white"),
      ))
    )(converterParams.block.prov))

}

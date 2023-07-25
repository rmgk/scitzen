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

object PikchrModule extends SvgViewboxModule("pikchr", process"pikchr --svg-only -")
object D2Module     extends SvgViewboxModule("d2", process"d2 - -")

abstract class SvgViewboxModule(override val handles: String, processBuilder: ProcessBuilder)
    extends BlockConverterModule {

  val viewboxRegex = raw"""viewBox="(?<left>[\d\.]+)\s+(?<top>[\d\.]+)\s+(?<right>[\d\.]+)\s+(?<bottom>[\d\.]+)"""".r

  override def convert(converterParams: ConverterParams): List[Sast] =

    val bytes  = converterParams.content.getBytes(StandardCharsets.UTF_8)
    val name   = Hashes.sha1hex(bytes)
    val target = converterParams.project.cachePath(Path.of(s"$name/$name.svg"))
    if !Files.exists(target.absolute) then
      Files.createDirectories(target.absolute.getParent)
      val start = System.nanoTime()
      val pikchr = processBuilder
        .redirectError(Redirect.DISCARD)
        .start()
      Using.resource(pikchr.getOutputStream) { os => os.write(bytes) }
      val svg = Using.resource(pikchr.getInputStream) { _.readToString }
      if pikchr.waitFor() != 0
      then
        cli.warn(s"${handles} compilation returned error code:")
        cli.warn(svg.indent(2))
      val updated =
        try
          val res    = viewboxRegex.findFirstMatchIn(svg).get
          val width  = res.group("right").toDouble - res.group("left").toDouble
          val height = res.group("bottom").toDouble - res.group("top").toDouble
          svg.replaceFirst("""<svg""", s"""<svg width="${width}" height="$height"""")
        catch
          case NonFatal(e) =>
            cli.warn(s"could not find viewbox during ${handles} transformation", e)
            svg
      Files.writeString(target.absolute, updated, StandardCharsets.UTF_8)
      cli.info(s"${handles} compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    List(Directive(
      DCommand.Image,
      Attributes(converterParams.block.attributes.raw ++ Seq(
        Attribute(target.projectAbsolute.toString),
        Attribute("color", "autoinvert")
      ))
    )(converterParams.block.prov))

}

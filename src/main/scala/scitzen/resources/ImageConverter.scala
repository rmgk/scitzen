package scitzen.resources

import de.rmgk.delay.*
import de.rmgk.script.*
import scitzen.generic.ProjectPath

import scala.util.Try

trait ImageConverter(val produces: Filetype)(val accepts: Filetype*):
  def convert(input: ProjectPath, output: ProjectPath): Async[Any, Boolean]

object ImageConverter:
  class Cairosvg(produces: Filetype) extends ImageConverter(produces)(Filetype.svg):
    override def convert(input: ProjectPath, output: ProjectPath): Async[Any, Boolean] = Async:
      val res: Process =
        process"cairosvg ${input.absolute} -f ${produces.extension} -o ${output.absolute}".start().onExit().toAsync.bind
      res.exitValue() == 0

  class ImageMagick(accepts: Filetype*)(produces: Filetype) extends ImageConverter(produces)(accepts*):
    override def convert(input: ProjectPath, output: ProjectPath): Async[Any, Boolean] = Async:
      val res: Process =
        process"convert ${input.absolute} ${output.absolute}".start().onExit().toAsync.bind
      res.exitValue() == 0

  class Pdftocairo(produces: Filetype) extends ImageConverter(produces)(Filetype.pdf):
    override def convert(input: ProjectPath, output: ProjectPath): Async[Any, Boolean] =
      Async:
        val res: Process =
          process"""pdftocairo
            ${Option.unless(produces == Filetype.svg)("-singlefile")}
            ${"-" + produces.extension} ${input.absolute} ${output.absolute}""".start().onExit().toAsync.bind
        res.exitValue() == 0

  def requiresCommand(command: String)(services: ImageConverter*): Seq[ImageConverter] =
    if Try(process"command -q $command".start.waitFor() == 0).getOrElse(false) then Nil
    else services

  val enabledConversions: Seq[ImageConverter] = Seq(
    requiresCommand("cairosvg")(Cairosvg(Filetype.pdf), Cairosvg(Filetype.png)),
    requiresCommand("pdftocairo")(Pdftocairo(Filetype.svg), Pdftocairo(Filetype.png)),
    requiresCommand("convert")(ImageMagick(Filetype.webp)(Filetype.jpg))
  ).flatten

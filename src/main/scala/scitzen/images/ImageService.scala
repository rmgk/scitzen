package scitzen.images

import de.rmgk.delay.*
import de.rmgk.script.*
import scitzen.generic.ProjectPath

import scala.util.Try

trait ImageService(val produces: Filetype)(val accepts: Filetype*):
  def convert(input: ProjectPath, output: ProjectPath): Async[Any, Boolean]

object ImageService:
  class Cairosvg(produces: Filetype) extends ImageService(produces)(Filetype.svg):
    override def convert(input: ProjectPath, output: ProjectPath): Async[Any, Boolean] = Async:
      val res: Process =
        process"cairosvg ${input.absolute} -f ${produces.extension} -o ${output.absolute}".start().onExit().toAsync.bind
      res.exitValue() == 0

  class ImageMagick(accepts: Filetype*)(produces: Filetype) extends ImageService(produces)(accepts*):
    override def convert(input: ProjectPath, output: ProjectPath): Async[Any, Boolean] = Async:
      val res: Process =
        process"convert ${input.absolute} ${output.absolute}".start().onExit().toAsync.bind
      res.exitValue() == 0

  class Pdftocairo(produces: Filetype) extends ImageService(produces)(Filetype.pdf):
    override def convert(input: ProjectPath, output: ProjectPath): Async[Any, Boolean] =
      Async:
        val res: Process =
          process"""pdftocairo
            ${Option.unless(produces == Filetype.svg)("-singlefile")}
            ${"-" + produces.extension} ${input.absolute} ${output.absolute}""".start().onExit().toAsync.bind
        res.exitValue() == 0

  def requiresCommand(command: String)(services: ImageService*): Seq[ImageService] =
    if Try(process"command -q $command".start.waitFor() == 0).getOrElse(false) then Nil
    else services

  val enabledConversions: Seq[ImageService] = Seq(
    requiresCommand("cairosvg")(Cairosvg(Filetype.pdf), Cairosvg(Filetype.png)),
    requiresCommand("pdftocairo")(Pdftocairo(Filetype.svg), Pdftocairo(Filetype.png)),
    requiresCommand("convert")(ImageMagick(Filetype.webp)(Filetype.jpg))
  ).flatten

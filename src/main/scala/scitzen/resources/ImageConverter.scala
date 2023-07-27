package scitzen.resources

import de.rmgk.delay.*
import de.rmgk.script.*
import scitzen.project.ProjectPath

import scala.util.Try

trait ImageConverter(val produces: Filetype)(val accepts: Filetype*):
  def convert(input: ProjectPath, output: ProjectPath): Async[Any, Boolean]

object ImageConverter:
  def requiresCommand(command: String)(services: ImageConverter*): Seq[ImageConverter] =
    if Try(process"command -q $command".start.waitFor() == 0).getOrElse(false) then Nil
    else services

  val enabledConversions: Seq[ImageConverter] = Seq(
    requiresCommand("cairosvg")(Cairosvg(Filetype.pdf), Cairosvg(Filetype.png)),
    requiresCommand("pdftocairo")(Pdftocairo(Filetype.svg), Pdftocairo(Filetype.png)),
    requiresCommand("convert")(
      ImageMagick(Filetype.webp)(Filetype.jpg),
      ImageMagickAnimation(Filetype.mp4, Filetype.gif)(Filetype.jpg)
    )
  ).flatten

class Cairosvg(produces: Filetype) extends ImageConverter(produces)(Filetype.svg):
  override def convert(input: ProjectPath, output: ProjectPath): Async[Any, Boolean] = Async:
    val res: Process =
      process"cairosvg --background white ${input.absolute} -f ${produces.extension} -o ${output.absolute}".start().onExit().toAsync.bind
    res.exitValue() == 0

class ImageMagick(accepts: Filetype*)(produces: Filetype) extends ImageConverter(produces)(accepts*):
  override def convert(input: ProjectPath, output: ProjectPath): Async[Any, Boolean] = Async:
    val res: Process =
      process"convert ${input.absolute} ${output.absolute}".start().onExit().toAsync.bind
    res.exitValue() == 0

class ImageMagickAnimation(accepts: Filetype*)(produces: Filetype) extends ImageConverter(produces)(accepts*):
  override def convert(input: ProjectPath, output: ProjectPath): Async[Any, Boolean] = Async:
    val res: Process =
      process"convert ${input.absolute.toString + "[0]"} ${output.absolute}".start().onExit().toAsync.bind
    res.exitValue() == 0

class Pdftocairo(produces: Filetype) extends ImageConverter(produces)(Filetype.pdf):
  val isVector: Boolean = produces match
    case Filetype.svg | Filetype.pdf => true
    case other                       => false
  override def convert(input: ProjectPath, output: ProjectPath): Async[Any, Boolean] =
    Async:
      val stripedOut =
        if isVector
        then output.absolute.toString
        else output.absolute.toString.stripSuffix(s".${produces.extension}")
      val res: Process =
        process"""pdftocairo
                ${Option.unless(isVector)("-singlefile")}
                ${"-" + produces.extension} ${input.absolute} ${stripedOut}""".start().onExit().toAsync.bind
      res.exitValue() == 0

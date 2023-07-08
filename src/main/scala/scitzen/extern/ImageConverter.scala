package scitzen.extern

import de.rmgk.delay.Async
import scitzen.compat.Logging
import scitzen.generic.{Project, ProjectPath}
import de.rmgk.script.*
import de.rmgk.delay.extensions
import de.rmgk.script.extensions.process
import scitzen.compat.Logging.{cli, given}

import scala.util.{Try, boundary}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** like a mime type, but worse */
enum Filetype(val extension: String, val aliases: String*):
  case svg  extends Filetype("svg")
  case png  extends Filetype("png")
  case pdf  extends Filetype("pdf")
  case webp extends Filetype("webp")
  case jpg  extends Filetype("jpg", "jpeg")

object Filetype:
  val all: List[Filetype]           = List(svg, png, pdf, webp, jpg)
  val lookup: Map[String, Filetype] = all.flatMap(ft => (ft.extension, ft) +: ft.aliases.map(al => (al, ft))).toMap

  def nameWithoutExtension(p: Path): String =
    val filename = p.getFileName.toString
    val ext      = filename.lastIndexOf('.')
    if ext >= 0
    then filename.substring(0, ext)
    else filename

  def of(p: Path): Option[Filetype] =
    val filename = p.getFileName.toString
    val ext      = filename.lastIndexOf('.')
    if ext >= 0
    then Filetype.lookup.get(filename.substring(ext + 1, filename.length))
    else None

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

class ConversionDispatch(project: Project, imageTarget: ImageTarget):

  /** For each possible input type, finds a converter with the preferred output type */
  val conversionChoice: Map[Filetype, ImageService] =
    Filetype.all.flatMap: inputType =>
      val candidates = ImageService.enabledConversions.filter: is =>
        is.accepts.contains(inputType)
      boundary:
        imageTarget.choices.foreach: accepted =>
          candidates.collectFirst: can =>
            if can.produces == accepted
            then boundary.break(Some(inputType -> can))
        None
    .toMap
  end conversionChoice

  def converterFor(input: ProjectPath): Option[ImageService] =
    Filetype.of(input.absolute).flatMap(conversionChoice.get)

  def predictTarget(input: ProjectPath): Option[ProjectPath] =
    converterFor(input).map(c => predictTargetOf(input, c.produces))

  def predictTargetOf(input: ProjectPath, targetType: Filetype): ProjectPath =
    val targetFilename =
      s"${input.absolute.getFileName.toString}.${targetType.extension}"
    val targetPath = if input.absolute.startsWith(project.cacheDir) then
      input.absolute.resolveSibling(targetFilename)
    else
      val relative = project.root.relativize(input.absolute)
      project.cacheDir.resolve("convertedImages")
        .resolve(relative)
        .resolve(targetFilename)

    ProjectPath(project, targetPath)

  def convert(input: ProjectPath) =
    val absoluteInput = input.absolute
    converterFor(input) match
      case None =>
        Logging.cli.warn(s"unknown file ending", absoluteInput)
      case Some(converter) =>
        val targetfile = predictTargetOf(input, converter.produces).absolute

        val sourceModified = Files.getLastModifiedTime(absoluteInput)
        if !Files.exists(targetfile) || Files.getLastModifiedTime(targetfile) != sourceModified then
          Files.createDirectories(targetfile.getParent)
          Logging.cli.trace(s"converting $input to $targetfile")

          Files.setLastModifiedTime(targetfile, Files.getLastModifiedTime(absoluteInput))
          ()
        targetfile

class ImagePaths(project: Project):
  val html   = ConversionDispatch(project, ImageTarget.Html)
  val tex    = ConversionDispatch(project, ImageTarget.Tex)
  val raster = ConversionDispatch(project, ImageTarget.Raster)
  def lookup(imageTarget: ImageTarget): ConversionDispatch = imageTarget match
    case ImageTarget.Html   => html
    case ImageTarget.Tex    => tex
    case ImageTarget.Raster => raster

enum ImageTarget(
    val name: String,
    val preferredFormat: String,
    val alternative: List[String],
    val unsupportedFormat: List[String]
):
  def requiresConversion(filename: ProjectPath): Boolean =
    unsupportedFormat.exists(fmt => filename.absolute.toString.endsWith(fmt))
  def choices: List[Filetype] = (preferredFormat :: alternative).flatMap(Filetype.lookup.get)
  case Html   extends ImageTarget("html target", "svg", Nil, List("pdf", "tex"))
  case Tex    extends ImageTarget("tex target", "pdf", List("jpg"), List("svg", "tex", "webp"))
  case Raster extends ImageTarget("raster target", "png", Nil, List("svg", "pdf", "tex"))

case class ImageConversions(mapping: Map[ProjectPath, Map[ImageTarget, ProjectPath]]):
  def lookup(path: ProjectPath, target: ImageTarget): ProjectPath =
    mapping.get(path).flatMap(_.get(target)).getOrElse(path)

//object ImageConverter {
//
//  def preprocessImages(
//      project: Project,
//      targets: List[ImageTarget],
//      paths: Iterable[ProjectPath]
//  ): ImageConversions =
//    val converters = targets.map(t => ImageConverter.apply(project, t))
//    ImageConversions:
//      paths.map: path =>
//        path -> converters.flatMap: conv =>
//          conv.applyConversion(path).map: res =>
//            conv.imageTarget -> project.asProjectPath(res)
//        .toMap
//      .toMap
//}

package scitzen.extern

import scitzen.compat.Logging.cli
import scitzen.generic.{Project, ProjectPath}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

trait ImageService(val accepts: Set[String], val produces: String):
  def convert(input: ProjectPath): Path

enum ImageTarget(val name: String, val preferredFormat: String, val alternative: List[String], val unsupportedFormat: List[String]):
  def requiresConversion(filename: ProjectPath): Boolean =
    unsupportedFormat.exists(fmt => filename.absolute.toString.endsWith(fmt))
  case Html   extends ImageTarget("html target", "svg", Nil, List("pdf", "tex"))
  case Tex    extends ImageTarget("tex target", "pdf", List("jpg"), List("svg", "tex", "webp"))
  case Raster extends ImageTarget("raster target", "png", Nil, List("svg", "pdf", "tex"))

case class ImageConversions(mapping: Map[ProjectPath, Map[ImageTarget, ProjectPath]]):
  def lookup(path: ProjectPath, target: ImageTarget): ProjectPath =
    mapping.get(path).flatMap(_.get(target)).getOrElse(path)

object ImageConverter {

  def nameWithoutExtension(p: Path): String =
    val filename = p.getFileName.toString
    val ext      = filename.lastIndexOf('.')
    if ext >= 0
    then filename.substring(0, ext)
    else filename

  def preprocessImages(
      project: Project,
      targets: List[ImageTarget],
      paths: Iterable[ProjectPath]
  ): ImageConversions =
    val converters = targets.map(t => ImageConverter.apply(project, t))
    ImageConversions:
      paths.map: path =>
        path -> converters.flatMap: conv =>
          conv.applyConversion(path).map: res =>
            conv.imageTarget -> project.asProjectPath(res)
        .toMap
      .toMap
}

case class ImageConverter(
    project: Project,
    imageTarget: ImageTarget,
):

  def preferredFormat: String = imageTarget.preferredFormat

  def applyConversion(file: ProjectPath): Option[Path] =
    val filename = file.absolute.getFileName.toString
    if imageTarget.requiresConversion(file)
    then
      preferredFormat match
        case "svg" | "png" if (filename.endsWith(".pdf")) => Some(pdfToCairo(file.absolute))
        case "pdf" | "png" if (filename.endsWith(".svg")) => Some(svgToCairo(file.absolute))
        case other                                                                  =>
          if imageTarget.alternative.contains("jpg")
          then Some(imageMagickToJpg(file.absolute))
          else None


    else None

  def imageMagickToJpg(file: Path): Path =
    convertExternal(file, "jpg"):
      (source, target) =>
        List(
          "convert",
          source.toAbsolutePath.toString,
          target.toAbsolutePath.toString
        )

  def pdfToCairo(file: Path): Path =
    convertExternal(
      file, preferredFormat):
      (source, target) =>
        preferredFormat match
          case "png" | "jpeg" | "tiff" =>
            List(
              "pdftocairo",
              "-singlefile",
              s"-$preferredFormat",
              source.toAbsolutePath.toString,
              target.resolveSibling(ImageConverter.nameWithoutExtension(target)).toAbsolutePath.toString
            )
          case "svg" =>
            List("pdftocairo", s"-$preferredFormat", source.toAbsolutePath.toString, target.toAbsolutePath.toString)

  def svgToCairo(file: Path): Path =
    convertExternal(
      file, preferredFormat):
      (source, target) => {
        List("cairosvg", source.toAbsolutePath.toString, "-o", target.toAbsolutePath.toString)
      }

  trait CommandFunction:
    def genCommand(source: Path, target: Path): List[String]

  private def convertExternal(file: Path, extension: String)(command: CommandFunction): Path =
    val relative       = project.root.relativize(file)
    val targetfileName = ImageConverter.nameWithoutExtension(file) + s".$extension"
    val targetfile =
      if file.startsWith(project.cacheDir) then
        file.resolveSibling(targetfileName)
      else
        project.cacheDir.resolve("convertedImages")
          .resolve(relative)
          .resolve(targetfileName)
    val sourceModified = Files.getLastModifiedTime(file)
    if !Files.exists(targetfile) || Files.getLastModifiedTime(targetfile) != sourceModified then
      Files.createDirectories(targetfile.getParent)
      cli.trace(s"converting $file to $targetfile")
      new ProcessBuilder(command.genCommand(file, targetfile).asJava)
        .inheritIO().start().waitFor()
      Files.setLastModifiedTime(targetfile, Files.getLastModifiedTime(file))
      ()
    targetfile

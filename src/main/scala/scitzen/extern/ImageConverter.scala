package scitzen.extern

import scitzen.compat.Logging.scribe
import scitzen.generic.{ArticleDirectory, Project}
import scitzen.sast.{Attributes}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

object ImageConverter {

  def nameWithoutExtension(p: Path): String =
    val filename = p.getFileName.toString
    val ext      = filename.lastIndexOf('.')
    if ext >= 0
    then filename.substring(0, ext)
    else filename

  def preprocessImages(
      project: Project,
      targets: List[ImageTarget] = Nil,
      articleDirectory: ArticleDirectory
  ): Unit =
    val converters = targets.map(t => t -> new ImageConverter(project, t)).toMap
//    val blocks = articleDirectory.articles.flatMap { art =>
//      art.context.convertBlocks.map(b => b.attributes -> (b -> art.sourceDoc))
//    }.toMap

//    blocks.valuesIterator.foreach { (block, doc) =>
//      converters.foreach { (_, ic) =>
//        ic.convertBlock(doc.path.directory, block)
//      }
//    }

    val macros = articleDirectory.articles.flatMap { art =>
      art.context.imageMacros.map(m => m.attributes -> (m -> art.sourceDoc))
    }.toMap

    macros.valuesIterator.foreach { (mcro, doc) =>
      val file = project.resolve(doc.path.directory, mcro.attributes.target)
      converters.foreach { (t, _) =>
        if t.requiresConversion(mcro.attributes.target) && file.isDefined then
          converters(t).applyConversion(file.get.absolute, mcro.attributes).map(f => (t -> f))
          ()
      }
    }
  end preprocessImages


}

class ImageConverter(
    project: Project,
    imageTarget: ImageTarget,
):

  def preferredFormat: String         = imageTarget.preferredFormat

  def applyConversion(file: Path, attributes: Attributes): Option[Path] =
    preferredFormat match
      case "svg" | "png" if (file.getFileName.toString.endsWith(".pdf")) => Some(pdfToCairo(file))
      case "pdf" | "png" if (file.getFileName.toString.endsWith(".svg")) => Some(svgToCairo(file))
//      case _ if (file.getFileName.toString.endsWith(".tex")) =>
//        val dir = project.cacheDir.resolve("convertedImages").resolve(project.root.relativize(file))
//        val templated =
//          ImageConverter.applyTemplate(attributes, Files.readString(file), file.getParent, project, documentDirectory)
//        convertTemplated("tex", templated, dir, ImageConverter.nameWithoutExtension(file))

  def pdfToCairo(file: Path): Path =
    convertExternal(
      file,
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
    )

  def svgToCairo(file: Path): Path =
    convertExternal(
      file,
      (source, target) => {
        List("cairosvg", source.toAbsolutePath.toString, "-o", target.toAbsolutePath.toString)
      }
    )

  trait CommandFunction:
    def genCommand(source: Path, target: Path): List[String]

  private def convertExternal(file: Path, command: CommandFunction): Path =
    val relative       = project.root.relativize(file)
    val targetfileName = ImageConverter.nameWithoutExtension(file) + s".$preferredFormat"
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
      scribe.debug(s"converting $file to $targetfile")
      new ProcessBuilder(command.genCommand(file, targetfile).asJava)
        .inheritIO().start().waitFor()
      Files.setLastModifiedTime(targetfile, Files.getLastModifiedTime(file))
      ()
    targetfile

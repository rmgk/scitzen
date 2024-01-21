package scitzen.resources

import de.rmgk.delay.Async
import scitzen.cli.Logging
import Logging.given
import scitzen.project.{Project, ProjectPath}

import java.nio.file.Files
import scala.util.boundary

class ConversionDispatch(project: Project, imageTarget: ImageTarget):

  /** For each possible input type, finds a converter with the preferred output type */
  val conversionChoice: Map[Filetype, ImageConverter] =
    Filetype.all.flatMap: inputType =>
      val candidates = ImageConverter.enabledConversions.filter: is =>
        is.accepts.contains(inputType)
      boundary:
        imageTarget.choices.foreach: accepted =>
          candidates.foreach: can =>
            if can.produces == accepted
            then boundary.break(Some(inputType -> can))
        None
    .toMap
  end conversionChoice

  def converterFor(input: ProjectPath): Option[ImageConverter] =
    Filetype.of(input.absolute).flatMap: ft =>
      conversionChoice.get(ft)

  def predictTarget(input: ProjectPath): Option[ProjectPath] =
    converterFor(input).map: c =>
      predictTargetOf(input, c.produces)

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

  def convert(input: ProjectPath): Async[Unit, Boolean] =
    val absoluteInput = input.absolute
    converterFor(input) match
      case None =>
        Logging.cli.warn(s"no converter to ${imageTarget.choices} from ", absoluteInput)
        Async(false)
      case Some(converter) =>
        val targetfile = predictTargetOf(input, converter.produces).absolute

        val sourceModified = Files.getLastModifiedTime(absoluteInput)
        if Files.exists(targetfile) && Files.getLastModifiedTime(targetfile) == sourceModified
        then Async(true)
        else
          Files.createDirectories(targetfile.getParent)
          Logging.cli.trace(s"converting $input to $targetfile")
          Async:
            val res = converter.convert(input, ProjectPath(project, targetfile)).bind
            if res
            then
              Files.setLastModifiedTime(targetfile, Files.getLastModifiedTime(absoluteInput))
              ()
            res

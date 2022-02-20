package scitzen.cli

import better.files.*
import cats.implicits.*
import com.monovore.decline.Visibility.Partial
import com.monovore.decline.{Command, CommandApp, Opts}
import scitzen.extern.{ImageConverter, ImageTarget}
import scitzen.generic.{PreprocessedResults, Project, ProjectConfig}
import scala.Option.when

import java.nio.file.{Path, Paths}

object Scitzen
    extends CommandApp(
      name = "scitzen",
      header = "Static page generator",
      main = ConvertProject.command.options
    )

object ConvertProject:

  val args = (
    Opts.argument[Path](metavar = "path").withDefault(Paths.get("")),
    Opts.option[Path]("sync-file", metavar = "file", visibility = Partial, help = "file to show in output").orNone,
    Opts.option[Int](
      "sync-position",
      metavar = "integer",
      visibility = Partial,
      help = "character offset to show in output"
    ).orNone,
    Opts.flag("image-file-map", visibility = Partial, help = "character offset to show in output").orFalse,
    Opts.option[Path](long = "json", metavar = "path", help = "print single file structure as json").orNone,
    Opts.flag("use-cats-parse", visibility = Partial, help = "use cats parse instead of fastparse").orFalse,
  )

  val command: Command[Unit] = Command(name = "gen", header = "Convert Scim to Sast.") {
    args.mapN {
      (sourcedirRel, syncFileRelOption, syncPos, imageFileMap, printJson, useCatsParse) =>
        printJson match
          case Some(path) =>
            println(JsonSast.jsonFor(File(path), Project(File(path).parent, ProjectConfig.parse("b=c"), Map.empty)))
          case None =>
            Project.fromSource(File(sourcedirRel)) match
              case None => scribe.error(s"could not find project for $sourcedirRel")
              case Some(project) =>
                executeConversions(syncFileRelOption, syncPos, imageFileMap, project, useCatsParse)
    }
  }

  def makeTimediff(): () => String =
    val starttime = System.nanoTime()
    var lasttime  = starttime
    def timediff: String =
      val now           = System.nanoTime()
      def diff(t: Long) = (now - t) / 1000000
      val res           = s"(${diff(starttime)}ms|${diff(lasttime)}ms)"
      lasttime = now
      res
    () => timediff

  private def executeConversions(
      syncFileRelOption: Option[Path],
      syncPos: Option[Int],
      imageFileMap: Boolean,
      project: Project,
      useCatsParse: Boolean,
  ): Unit =
    val timediff = makeTimediff()

    scribe.info(s"found project in ${project.root} ${timediff()}")
    val documentDirectory = Project.directory(project.root, useCatsParse)

    scribe.info(s"parsed ${documentDirectory.documents.size} documents ${timediff()}")

    val preprocessed = new PreprocessedResults(
      project,
      documentDirectory.documents
    )

    project.outputdir.createDirectories()

    val toHtml = project.config.outputType.contains("html")
    val toPdf  = project.config.outputType.contains("pdf")

    ImageConverter.preprocessImages(
      project,
      documentDirectory,
      List(
        when(toHtml)(ImageTarget.Html),
        when(toPdf)(ImageTarget.Tex),
        when(imageFileMap)(ImageTarget.Raster)
      ).flatten,
      preprocessed
    )

    if project.config.format.contains("content") then
      Format.formatContents(documentDirectory)
      scribe.info(s"formatted contents ${timediff()}")
    if project.config.format.contains("filename") then
      Format.formatRename(documentDirectory)
      scribe.info(s"formatted filenames ${timediff()}")
    if toHtml then
      val sync = syncFileRelOption.map2(syncPos)((f, p) => File(f) -> p)
      ConvertHtml.convertToHtml(project, sync, preprocessed)
      scribe.info(s"generated html ${timediff()}")
    if toPdf then
      ConvertPdf.convertToPdf(project, preprocessed)
      scribe.info(s"generated pdfs ${timediff()}")
    if imageFileMap then
      ImageReferences.listAll(project, documentDirectory)
      scribe.info(s"generated imagemap ${timediff()}")

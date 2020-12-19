package scitzen.cli

import better.files._
import cats.implicits._
import com.monovore.decline.Visibility.Partial
import com.monovore.decline.{Command, CommandApp, Opts}
import scitzen.generic.{DocumentDirectory, Project}
import scribe.Logger

import java.nio.file.{Path, Paths}

object Scitzen
    extends CommandApp(
      name = "scitzen",
      header = "Static page generator",
      main = {
        import scribe.format._
        val myFormatter: Formatter = formatter"$message ($position)"
        Logger.root.clearHandlers().withHandler(
          formatter = myFormatter,
          minimumLevel = Some(scribe.Level.Info)
        ).replace()

        ConvertProject.command.options
      }
    )

object ConvertProject {

  val args = (
    Opts.argument[Path](metavar = "path")
      .withDefault(Paths.get("")),
    Opts.option[Path]("sync-file", metavar = "file", visibility = Partial, help = "file to show in output").orNone,
    Opts.option[Int](
      "sync-position",
      metavar = "integer",
      visibility = Partial,
      help = "character offset to show in output"
    ).orNone,
    Opts.flag("image-file-map", visibility = Partial, help = "character offset to show in output").orFalse,
    Opts.option[Path](long = "json", metavar = "path", help = "print single file structure as json")
      .orNone,
  )

  val command: Command[Unit] = Command(name = "gen", header = "Convert Scim to Sast.") {
    args.mapN {
      (sourcedirRel, syncFileRelOption, syncPos, imageFileMap, printJson) =>
        printJson match {
          case Some(path) => println(JsonSast.jsonFor(File(path)))
          case None =>
            Project.fromSource(File(sourcedirRel)) match {
              case None => scribe.error(s"could not find project for $sourcedirRel")
              case Some(project) =>
                executeConversions(syncFileRelOption, syncPos, imageFileMap, project)
            }
        }
    }
  }

  def makeTimediff(): () => String = {
    val starttime = System.nanoTime()
    var lasttime  = starttime
    def timediff: String = {
      val now           = System.nanoTime()
      def diff(t: Long) = (now - t) / 1000000
      val res           = s"(${diff(starttime)}ms|${diff(lasttime)}ms)"
      lasttime = now
      res
    }
    () => timediff
  }

  private def executeConversions(
      syncFileRelOption: Option[Path],
      syncPos: Option[Int],
      imageFileMap: Boolean,
      project: Project
  ): Unit = {
    val timediff = makeTimediff()

    scribe.info(s"found project in ${project.root} ${timediff()}")
    val documentDirectory = DocumentDirectory(project.root)
    scribe.info(s"parsed ${documentDirectory.documents.size} documents ${timediff()}")
    if (project.config.format.contains("content")) {
      Format.formatContents(documentDirectory)
      scribe.info(s"formatted contents ${timediff()}")
    }
    if (project.config.format.contains("filename")) {
      Format.formatRename(documentDirectory)
      scribe.info(s"formatted filenames ${timediff()}")
    }
    if (project.config.outputType.contains("html")) {
      val sync = syncFileRelOption.map2(syncPos)((f, p) => File(f) -> p)
      ConvertHtml.convertToHtml(project, sync, documentDirectory)
      scribe.info(s"generated html ${timediff()}")
    }
    if (project.config.outputType.contains("pdf")) {
      ConvertPdf.convertToPdf(project, documentDirectory)
      scribe.info(s"generated pdfs ${timediff()}")
    }
    if (imageFileMap) {
      ImageReferences.listAll(project, documentDirectory)
      scribe.info(s"generated imagemap ${timediff()}")
    }
  }
}

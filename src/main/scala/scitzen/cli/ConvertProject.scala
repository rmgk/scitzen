package scitzen.cli

import scitzen.bibliography.{BibDB, BibManager}
import scitzen.cli.ScitzenCommandline.ClSync
import scitzen.compat.Logging.scribe
import scitzen.extern.{ImageConverter, ImageTarget}
import scitzen.generic.{PreprocessedResults, Project}

import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object ConvertProject:

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

  def executeConversions(
      sync: Option[ClSync],
      imageFileMap: Option[Path],
      project: Project,
  ): Unit =
    val timediff = makeTimediff()

    scribe.info(s"found project in ${project.root} ${timediff()}")
    val documentDirectory = Project.directory(project.root)

    scribe.info(s"parsed ${documentDirectory.documents.size} documents ${timediff()}")

    val preprocessed = new PreprocessedResults(
      project,
      documentDirectory.documents
    )

    Files.createDirectories(project.outputdir)

    val toHtml = project.config.outputType.contains("html")
    val toPdf  = project.config.outputType.contains("pdf")

    val bibres = BibManager(project).prefetch(preprocessed.preprocessedCtxs.iterator.flatMap(_.citations).toSet)
    val dblpFuture = Future { bibres.runToFuture }.flatten

    ImageConverter.preprocessImages(
      project,
      documentDirectory,
      List(
        Option.when(toHtml)(ImageTarget.Html),
        Option.when(toPdf)(ImageTarget.Tex),
        imageFileMap.map(_ => ImageTarget.Raster)
      ).flatten,
      preprocessed
    )

    val bibdb: BibDB = Await.result(dblpFuture, 30.seconds)


    if project.config.format.contains("content") then
      Format.formatContents(documentDirectory, bibdb)
      scribe.info(s"formatted contents ${timediff()}")
    if project.config.format.contains("filename") then
      Format.formatRename(documentDirectory)
      scribe.info(s"formatted filenames ${timediff()}")
    if toHtml then
      ConvertHtml.convertToHtml(project, sync, preprocessed, bibdb)
      scribe.info(s"generated html ${timediff()}")
    if toPdf then
      ConvertPdf.convertToPdf(project, preprocessed, bibdb)
      scribe.info(s"generated pdfs ${timediff()}")
    if imageFileMap.isDefined then
      ImageReferences.listAll(project, documentDirectory, imageFileMap.get)
      scribe.info(s"generated imagemap ${timediff()}")

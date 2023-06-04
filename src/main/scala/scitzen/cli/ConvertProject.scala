package scitzen.cli

import scitzen.bibliography.{BibEntry, Bibtex, DBLP}
import scitzen.cli.ScitzenCommandline.ClSync
import scitzen.extern.{ImageConverter, ImageTarget}
import scitzen.generic.{PreprocessedResults, Project}
import scitzen.compat.Logging.scribe

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, OpenOption, Path, StandardOpenOption}
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, DurationInt}

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

    val dblpcachePath: Path = project.cacheDir.resolve("dblpcache.bib")
    def parsebib(): Map[String, BibEntry] =
      (if Files.exists(dblpcachePath) then
         Bibtex.makeBib(dblpcachePath)
       else
         Map.empty
      ) ++
      project.bibfile.map(Bibtex.makeBib).getOrElse(Map.empty)
    val currentBib = parsebib()

    val dblpFuture =
      if toHtml || toPdf then
        val oldbib = dblpcachePath

        val citeKeys =
          for
            context   <- preprocessed.preprocessedCtxs.iterator
            directive <- context.citations.iterator
            citation  <- directive.attributes.target.split(',').iterator
          yield citation.trim
        val allCitations = citeKeys.toSet
        val missing      = allCitations -- currentBib.keySet
        val dblp         = missing.filter(_.startsWith("DBLP:"))
        if dblp.nonEmpty then
          scribe.info(s"scheduling download of ${dblp.size} missing citations")
          Future:
            dblp.flatMap: key =>
              DBLP.lookup(key.stripPrefix("DBLP:")).map: res =>
                Files.writeString(dblpcachePath, res, StandardCharsets.UTF_8, StandardOpenOption.APPEND, StandardOpenOption.CREATE)
        else Future.successful(())
      else Future.successful(())

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

    if project.config.format.contains("content") then
      Format.formatContents(documentDirectory)
      scribe.info(s"formatted contents ${timediff()}")
    if project.config.format.contains("filename") then
      Format.formatRename(documentDirectory)
      scribe.info(s"formatted filenames ${timediff()}")
    if !dblpFuture.isCompleted then
      scribe.info(s"awaiting DBLP results")
      Await.ready(dblpFuture, 10.seconds)
    if toHtml then
      ConvertHtml.convertToHtml(project, sync, preprocessed, parsebib())
      scribe.info(s"generated html ${timediff()}")
    if toPdf then
      ConvertPdf.convertToPdf(project, preprocessed)
      scribe.info(s"generated pdfs ${timediff()}")
    if imageFileMap.isDefined then
      ImageReferences.listAll(project, documentDirectory, imageFileMap.get)
      scribe.info(s"generated imagemap ${timediff()}")

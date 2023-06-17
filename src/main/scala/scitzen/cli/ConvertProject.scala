package scitzen.cli

import scitzen.bibliography.{BibDB, BibManager}
import scitzen.cli.ScitzenCommandline.ClSync
import scitzen.compat.Logging.scribe
import scitzen.extern.{BlockConverter, ImageConverter, ImageTarget}
import scitzen.generic.{ArticleDirectory, ArticleProcessing, Project}

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

    scribe.info(s"found project in ${Path.of("").toAbsolutePath.relativize(project.root)} ${timediff()}")
    val documents = ArticleProcessing.loadDocuments(project)
    val articles = ArticleDirectory:
      documents.flatMap: doc =>
        ArticleProcessing.processArticles(doc, project)

    scribe.info(s"parsed ${documents.size} documents ${timediff()}")

    Files.createDirectories(project.outputdir)

    val toHtml = project.config.outputType.contains("html")
    val toPdf  = project.config.outputType.contains("pdf")

    val bibres     = BibManager(project).prefetch(articles.articles.flatMap(_.context.citations).toSet)
    val dblpFuture = Future { bibres.runToFuture }.flatten

    val blockConversions = BlockConverter(project).apply(articles.articles)

    ImageConverter.preprocessImages(
      project,
      List(
        Option.when(toHtml)(ImageTarget.Html),
        Option.when(toPdf)(ImageTarget.Tex),
        imageFileMap.map(_ => ImageTarget.Raster)
      ).flatten,
      articles
    )

    val bibdb: BibDB = Await.result(dblpFuture, 30.seconds)

    if project.config.format.contains("content") then
      Format.formatContents(articles, bibdb)
      scribe.info(s"formatted contents ${timediff()}")
    if project.config.format.contains("filename") then
      Format.formatRename(articles)
      scribe.info(s"formatted filenames ${timediff()}")
    if toHtml then
      ConvertHtml(project, blockConversions).convertToHtml(sync, articles, bibdb)
      scribe.info(s"generated html ${timediff()}")
    if toPdf then
      ConvertPdf.convertToPdf(project, articles, bibdb)
      scribe.info(s"generated pdfs ${timediff()}")
    if imageFileMap.isDefined then
      ImageReferences.listAll(project, articles, imageFileMap.get)
      scribe.info(s"generated imagemap ${timediff()}")

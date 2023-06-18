package scitzen.cli

import scitzen.bibliography.{BibDB, BibManager}
import scitzen.cli.ScitzenCommandline.ClSync
import scitzen.compat.Logging.scribe
import scitzen.extern.{BlockConversions, BlockConverter, ImageConversions, ImageConverter, ImageTarget}
import scitzen.generic.{ArticleDirectory, ArticleProcessing, Project}
import scitzen.sast.{DCommand, Directive}

import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

case class ConversionAnalysis(
    project: Project,
    directory: ArticleDirectory,
    block: BlockConversions,
    image: ImageConversions,
    bib: BibDB,
)

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
    val directory = ArticleDirectory:
      documents.flatMap: doc =>
        ArticleProcessing.processArticles(doc, project)

    scribe.info(s"parsed ${documents.size} documents ${timediff()}")

    Files.createDirectories(project.outputdir)

    val toHtml = project.config.outputType.contains("html")
    val toPdf  = project.config.outputType.contains("pdf")

    val bibres     = BibManager(project).prefetch(directory.articles.flatMap(_.context.citations).toSet)
    val dblpFuture = Future { bibres.runToFuture }.flatten

    val blockConversions = BlockConverter(project, directory).run()

    val imagePaths =
      val bi = blockConversions.mapping.valuesIterator.flatten.collect:
        case Directive(DCommand.Image, attributes) => project.resolve(project.root, attributes.target)

      val di = directory.articles.iterator.flatMap: art =>
        art.context.imageDirectives.iterator.flatMap: d =>
          art.sourceDoc.resolve(d.attributes.target)

      bi.flatten.concat(di).toList

    val imageConversions = ImageConverter.preprocessImages(
      project,
      List(
        Option.when(toHtml)(ImageTarget.Html),
        Option.when(toPdf)(ImageTarget.Tex),
        imageFileMap.map(_ => ImageTarget.Raster)
      ).flatten,
      imagePaths
    )

    val bibdb: BibDB = Await.result(dblpFuture, 30.seconds)

    val anal = ConversionAnalysis(
      project = project,
      directory = directory,
      block = blockConversions,
      image = imageConversions,
      bib = bibdb
    )

    if project.config.format.contains("content") then
      Format.formatContents(anal)
      scribe.info(s"formatted contents ${timediff()}")
    if project.config.format.contains("filename") then
      Format.formatRename(directory)
      scribe.info(s"formatted filenames ${timediff()}")
    if toHtml then
      ConvertHtml(anal).convertToHtml(sync)
      scribe.info(s"generated html ${timediff()}")
    if toPdf then
      ConvertPdf.convertToPdf(anal)
      scribe.info(s"generated pdfs ${timediff()}")
    if imageFileMap.isDefined then
      ImageReferences.listAll(anal, imageFileMap.get)
      scribe.info(s"generated imagemap ${timediff()}")

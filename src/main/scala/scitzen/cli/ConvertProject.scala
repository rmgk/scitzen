package scitzen.cli

import scitzen.bibliography.{BibDB, BibManager}
import scitzen.cli.ScitzenCommandline.ClSync
import scitzen.compat.Logging.cli
import scitzen.extern.Katex.KatexLibrary
import scitzen.extern.{
  BlockConversions, BlockConverter, CachedConverterRouter, ImageConversions, ImageConverter, ImageTarget
}
import scitzen.generic.{ArticleDirectory, ArticleProcessing, Project}
import scitzen.sast.{DCommand, Directive}

import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.jdk.StreamConverters.*

case class ConversionAnalysis(
    project: Project,
    selectionPrefixes: List[Path],
    directory: ArticleDirectory,
    block: BlockConversions,
    image: ImageConversions,
    bib: BibDB,
    converter: Option[CachedConverterRouter],
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
      selection: List[Path],
  ): Unit =
    val timediff = makeTimediff()

    cli.info(s"found project in ./${Path.of("").toAbsolutePath.relativize(project.root)} ${timediff()}")
    val documents = ArticleProcessing.loadDocuments(project)

    val directory = ArticleDirectory:
      // note: tried to parallelize this, does not seem to be worth it in most cases
      documents.flatMap: doc =>
        ArticleProcessing.processArticles(doc)

    cli.info(s"parsed ${documents.size} documents ${timediff()}")

    Files.createDirectories(project.outputdir)

    val toHtml = project.config.outputType.contains("html")
    val toPdf  = project.config.outputType.contains("pdf")

    val bibres     = BibManager(project).prefetch(directory.articles.flatMap(_.context.citations).toSet)
    val dblpFuture = Future { bibres.runToFuture(using ()) }.flatten

    cli.info(s"scheduled bib ${timediff()}")

    val blockConversions = BlockConverter(project, directory).run()

    cli.info(s"block converted ${blockConversions.mapping.size} ${timediff()}")

    val imagePaths =
      val bi = blockConversions.mapping.valuesIterator.flatten.collect:
        case Directive(DCommand.Image, attributes) => project.resolve(project.root, attributes.target)

      val di = directory.articles.iterator.flatMap: art =>
        art.context.imageDirectives.iterator.flatMap: d =>
          art.doc.resolve(d.attributes.target)

      bi.flatten.concat(di).toList

    cli.info(s"image paths collected ${imagePaths.size} ${timediff()}")

    val imageConversions = ImageConverter.preprocessImages(
      project,
      List(
        Option.when(toHtml)(ImageTarget.Html),
        Option.when(toPdf)(ImageTarget.Tex),
        imageFileMap.map(_ => ImageTarget.Raster)
      ).flatten,
      imagePaths
    )

    cli.info(s"images converted ${imageConversions.mapping.size} ${timediff()}")

    val bibdb: BibDB = Await.result(dblpFuture, 30.seconds)

    cli.info(s"awaited bib ${bibdb.entries.size} ${bibdb.queried.size} ${timediff()}")

    val cachedConverter = new CachedConverterRouter(
      project.cacheDir.resolve("inlineConverter.json"),
      KatexLibrary(project.config.katexMacros.flatMap(project.resolve(project.root, _)))
    )

    val anal = ConversionAnalysis(
      project = project,
      selectionPrefixes = selection,
      directory = directory,
      block = blockConversions,
      image = imageConversions,
      bib = bibdb,
      converter = Some(cachedConverter)
    )

    cli.info(s"completed conversions ${timediff()}")

    if project.config.format.contains("content") then
      Format.formatContents(anal)
      cli.info(s"formatted contents ${timediff()}")
    if project.config.format.contains("filename") then
      Format.formatRename(directory)
      cli.info(s"formatted filenames ${timediff()}")
    if toHtml then
      ConvertHtml(anal).convertToHtml(sync)
      cli.info(s"generated html ${timediff()}")
    if toPdf then
      ConvertPdf.convertToPdf(anal)
      cli.info(s"generated pdfs ${timediff()}")
    if imageFileMap.isDefined then
      ImageReferences.listAll(anal, imageFileMap.get)
      cli.info(s"generated imagemap ${timediff()}")

    cachedConverter.writeCache()

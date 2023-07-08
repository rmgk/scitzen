package scitzen.cli

import scitzen.bibliography.{BibDB, BibManager}
import scitzen.cli.ScitzenCommandline.ClSync
import scitzen.compat.Logging.cli
import scitzen.extern.Katex.KatexLibrary
import scitzen.extern.{
  BlockConversions, BlockConverter, CachedConverterRouter, ImageConversions, ImageConverter, ImageTarget, ResourceUtil
}
import scitzen.generic.{ArticleDirectory, ArticleProcessing, Project, TitledArticle}
import scitzen.sast.{DCommand, Directive}

import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.jdk.StreamConverters.*

case class ConversionAnalysis(
    project: Project,
    selected: List[TitledArticle],
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

    Files.createDirectories(project.outputdir)
    if !Files.exists(project.pdfTemplatePath.absolute) then
      Files.createDirectories(project.pdfTemplatePath.absolute.getParent)
      val bytes = ResourceUtil.load("default-template.tex.scim")
      Files.write(project.pdfTemplatePath.absolute, bytes)
      ()

    val documents = ArticleProcessing.loadDocuments(project)

    val directory = ArticleDirectory(
      project,
      // note: tried to parallelize this, does not seem to be worth it in most cases
      documents.flatMap: doc =>
        ArticleProcessing.processArticles(doc)
    )

    cli.info(s"parsed ${documents.size} documents ${timediff()}")

    val dblpFuture = Future {
      val bibres = BibManager(project).prefetch(directory.articles.flatMap(_.context.citations).toSet)
      bibres.runToFuture(using ())
    }.flatten

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

    cli.trace(s"image paths collected ${imagePaths.size} ${timediff()}")

    val imageConversions = ImageConverter.preprocessImages(
      project,
      List(
        Some(ImageTarget.Html),
        Some(ImageTarget.Tex),
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

    val selected: List[TitledArticle] =
      directory.fullArticles.iterator.filter: art =>
        selection.exists: sel =>
          art.article.doc.path.absolute.startsWith(sel)
      .toList

    val anal = ConversionAnalysis(
      project = project,
      selected = selected,
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
    ConvertHtml(anal).convertToHtml(sync)
    cli.info(s"generated html ${timediff()}")
    ConvertPdf.convertToPdf(anal)
    cli.info(s"generated pdfs ${timediff()}")
    if imageFileMap.isDefined then
      ImageReferences.listAll(anal, imageFileMap.get)
      cli.info(s"generated imagemap ${timediff()}")

    cachedConverter.writeCache()

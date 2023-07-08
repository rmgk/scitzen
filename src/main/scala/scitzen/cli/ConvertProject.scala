package scitzen.cli

import de.rmgk.delay.Async
import scitzen.bibliography.{BibDB, BibManager}
import scitzen.cli.ScitzenCommandline.ClSync
import scitzen.compat.Logging.cli
import scitzen.extern.Katex.KatexLibrary
import scitzen.extern.{BlockConversions, BlockConverter, CachedConverterRouter, ImageTarget, ResourceUtil}
import scitzen.generic.{ArticleDirectory, ArticleProcessing, Project, TitledArticle}
import de.rmgk.delay.extensions.run
import scitzen.contexts.FileDependency

import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, Semaphore}
import scala.collection.Iterator.continually
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.jdk.StreamConverters.*

case class ConversionAnalysis(
    project: Project,
    selected: List[TitledArticle],
    directory: ArticleDirectory,
    block: BlockConversions,
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

    val htmlresult = ConvertHtml(anal).convertToHtml(sync)
    cli.info(s"generated html ${timediff()}")
    val pdfresult = ConvertPdf.convertToPdf(anal).toArray
    cli.info(s"generated tex ${timediff()}")
    if imageFileMap.isDefined then
      ImageReferences.listAll(anal, imageFileMap.get)
      cli.info(s"generated imagemap ${timediff()}")

    val convertees: Array[(FileDependency, ImageTarget)] =
      val htmldeps = htmlresult.iterator.zip(continually(ImageTarget.Tex))
      val pdfdeps  = pdfresult.iterator.flatMap(_.dependencies).zip(continually(ImageTarget.Tex))
      (htmldeps ++ pdfdeps).filter: (dep, _) =>
        dep.original != dep.file
      .toArray

    val okParallelism = math.max(Runtime.getRuntime.availableProcessors(), 4)

    {
      val cdl       = new CountDownLatch(convertees.size)
      val semaphore = new Semaphore(okParallelism)

      convertees.foreach: (dep, imageTarget) =>
        Async[Unit].resource(semaphore.acquire(), _ => { semaphore.release() }): _ =>
          project.imagePaths.lookup(imageTarget).convert(dep.original).bind
          val targetpath = dep.outputDirectory.absolute.resolve(dep.relativeFinalization)
          Files.createDirectories(targetpath.getParent)
          Files.deleteIfExists(targetpath)
          Files.createLink(targetpath, dep.file.absolute)
        .run(using ())(_ => cdl.countDown())
      cdl.await()
      cli.info(s"converted ${convertees.size} images ${timediff()}")
    }

    {
      val cdl       = new CountDownLatch(pdfresult.size)
      val semaphore = new Semaphore(okParallelism)
      pdfresult.foreach: pdftask =>
        Async.resource(semaphore.acquire(), _ => { semaphore.release() }): _ =>
          pdftask.task.bind
        .run(using ())(_ => cdl.countDown())
      cdl.await()
      cli.info(s"converted ${pdfresult.size} pdfs ${timediff()}")
    }

    cachedConverter.writeCache()

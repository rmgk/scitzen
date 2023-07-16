package scitzen.cli

import de.rmgk.delay.Async
import scitzen.bibliography.{BibDB, BibManager}
import scitzen.compat.Logging.cli
import scitzen.contexts.TargetedFileDependency
import scitzen.extern.Katex.KatexLibrary
import scitzen.extern.{BlockConversions, BlockConverter, CachedConverterRouter, ResourceUtil}
import scitzen.project.{ArticleDirectory, ArticleProcessing, Project, ProjectPath, TitledArticle}
import scitzen.resources.ImageTarget

import java.nio.file.{FileAlreadyExistsException, Files, Path}
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, Semaphore}
import scala.collection.Iterator.continually
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.jdk.StreamConverters.*

case class ConversionAnalysis(
    project: Project,
    directory: ArticleDirectory,
    block: BlockConversions,
    bib: BibDB,
    converter: Option[CachedConverterRouter],
)

object ConversionAnalysis:
  def minimal(project: Project, directory: ArticleDirectory): ConversionAnalysis =
    ConversionAnalysis(project, directory, BlockConversions(Map.empty), BibDB.empty, None)

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

    val bibmanagerFuture = Future(BibManager(project))

    val documents = ArticleProcessing.loadDocuments(project)

    val directory = ArticleDirectory(
      project.config.flags,
      // note: tried to parallelize this, does not seem to be worth it in most cases
      documents.flatMap: doc =>
        ArticleProcessing.processArticles(doc)
    )

    cli.info(s"parsed ${documents.size} documents ${timediff()}")

    val dblpFuture = bibmanagerFuture.map: bibmanager =>
      bibmanager.prefetch(directory.articles.flatMap(_.context.citations).toSet)
        .runToFuture(using ())
    .flatten

    cli.info(s"scheduled bib ${timediff()}")

    val blockConversions = BlockConverter(project, directory).run()

    cli.info(s"block converted ${blockConversions.mapping.size} ${timediff()}")

    val cachedConverter = new CachedConverterRouter(
      project.cacheDir.resolve("inlineConverter.json"),
      KatexLibrary(project.config.katexMacros.flatMap(project.resolve(project.root, _)))
    )

    val selected: List[TitledArticle] =
      directory.fullArticles.iterator.filter: art =>
        selection.exists: sel =>
          art.article.doc.path.absolute.startsWith(sel)
      .toList

    val bibdb: BibDB = Await.result(dblpFuture, 30.seconds)
    cli.info(s"awaited bib (entries ${bibdb.entries.size}) (queried ${bibdb.queried.size}) ${timediff()}")

    val anal = ConversionAnalysis(
      project = project,
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

    val htmlresult = ConvertHtml(anal).convertToHtml(selected)
    cli.info(s"generated html ${timediff()}")
    val pdfresult = ConvertPdf.convertToPdf(anal, selected).toArray
    cli.info(s"generated tex ${timediff()}")
    val ifmres =
      if imageFileMap.isEmpty
      then Nil
      else ImageReferences.listAll(anal, imageFileMap.get, selected)

    val convertees: Array[(TargetedFileDependency, ImageTarget)] =
      val htmldeps = htmlresult.iterator.zip(continually(ImageTarget.Html))
      val pdfdeps  = pdfresult.iterator.flatMap(_.dependencies).zip(continually(ImageTarget.Tex))
      val ifmdeps  = ifmres.iterator.zip(continually(ImageTarget.Raster))
      (htmldeps ++ pdfdeps ++ ifmdeps).toArray

    val okParallelism = math.max(Runtime.getRuntime.availableProcessors(), 4)

    {
      val cdl       = new CountDownLatch(convertees.size)
      val semaphore = new Semaphore(okParallelism)

      val converted = new ConcurrentHashMap[ProjectPath, Any](convertees.size * 2)

      convertees.foreach: (tdep, imageTarget) =>
        def dep = tdep.dep
        Async[Unit].resource(semaphore.acquire(), _ => { semaphore.release() }): _ =>
          val token = new {}
          Async.bind:
            if
              dep.file != dep.original &&
              converted.computeIfAbsent(dep.file, _ => token) == token
            then project.imagePaths.lookup(imageTarget).convert(dep.original)
            else Async(())
          val targetpath = tdep.outputPath.absolute.getParent.resolve(dep.relativeFinalization)
          require(targetpath != dep.original.absolute, "tried to overwrite file with itself")
          if targetpath != dep.file.absolute
          then
            Files.createDirectories(targetpath.getParent)
            Files.deleteIfExists(targetpath)
            try Files.createLink(targetpath, dep.file.absolute)
            catch case f: FileAlreadyExistsException => () // concurrency issue, ignore
            ()
        .run(using ())(_ => cdl.countDown())
      cdl.await()
      cli.info(s"ensured conversions for ${convertees.size} images ${timediff()}")
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

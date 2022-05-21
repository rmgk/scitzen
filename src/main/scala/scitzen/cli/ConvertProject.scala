package scitzen.cli

import scitzen.cli.ScitzenCommandline.ClSync
import scitzen.extern.{ImageConverter, ImageTarget}
import scitzen.generic.{PreprocessedResults, Project}
import scitzen.compat.Logging.scribe

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
      imageFileMap: Boolean,
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

    project.outputdir.createDirectories()

    val toHtml = project.config.outputType.contains("html")
    val toPdf  = project.config.outputType.contains("pdf")

    ImageConverter.preprocessImages(
      project,
      documentDirectory,
      List(
        Option.when(toHtml)(ImageTarget.Html),
        Option.when(toPdf)(ImageTarget.Tex),
        Option.when(imageFileMap)(ImageTarget.Raster)
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
      ConvertHtml.convertToHtml(project, sync, preprocessed)
      scribe.info(s"generated html ${timediff()}")
    if toPdf then
      ConvertPdf.convertToPdf(project, preprocessed)
      scribe.info(s"generated pdfs ${timediff()}")
    if imageFileMap then
      ImageReferences.listAll(project, documentDirectory)
      scribe.info(s"generated imagemap ${timediff()}")

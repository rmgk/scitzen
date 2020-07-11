package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}

import better.files._
import cats.data.Chain
import cats.implicits._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scitzen.extern.{Bibliography, ImageConverter, KatexConverter}
import scitzen.generic._
import scitzen.outputs.{GenIndexPage, HtmlPages, HtmlToc, SastToHtmlConverter, SastToSastConverter}
import scitzen.parser.MacroCommand.Cite
import scitzen.parser.Sast

import scala.util.Try
import scala.util.chaining._

object ConvertHtml {

  implicit val charset: Charset = StandardCharsets.UTF_8

  // loading ressource statically allows Graal AOT to inline on build
  val stylesheet: Array[Byte] = {
    Resource.asStream("scitzen.css").fold(File("scitzen.css").byteArray)(_.byteArray)
  }

  val mapCodec: JsonValueCodec[Map[String, String]] = JsonCodecMaker.make

  def convertToHtml(project: Project, sync: Option[(File, Int)]): Unit = {

    val documentManager = project.documentManager
    scribe.info(s"found ${documentManager.documents.size} documents")

    project.outputdir.createDirectories()

    val cssfile = project.outputdir / "scitzen.css"
    cssfile.writeByteArray(stylesheet)

    val nlp: Option[NLP] = if (project.nlpdir.isDirectory) Some(NLP.loadFrom(project.nlpdir, documentManager)) else None

    val katexmapfile    = project.cacheDir / "katexmap.json"
    val initialKatexMap = loadKatex(katexmapfile)

    val initialCtx = ConversionContext(Chain.empty[String], katexMap = initialKatexMap)

    import scala.jdk.CollectionConverters._
    val preprocessedCtxs: List[ConversionContext[(ParsedDocument, Chain[Sast])]] =
      documentManager.documents.asJava.parallelStream().map {
        preprocess(project, Some(KatexConverter(Some(katexmapfile))), initialCtx)
      }.iterator().asScala.toList
    val (preprocessed, articles) = splitPreprocessed(preprocessedCtxs)
    val preprocessedCtx          = preprocessedCtxs.foldLeft(initialCtx) { case (prev, next) => prev.merge(next) }
    writeKatex(katexmapfile, preprocessedCtx)

    val articleOutput = project.outputdir / "articles"
    articleOutput.createDirectories()
    val pathManager = HtmlPathManager(project.root, project, articleOutput)
    articles.foreach { article =>
      convertArticle(
        article,
        pathManager.changeWorkingFile(article.sourceDoc.file),
        project,
        cssfile,
        sync,
        preprocessed,
        preprocessedCtx,
        nlp
      )
    }

    val generatedIndex = GenIndexPage.makeIndex(documentManager, project, reverse = true)
    val convertedCtx =  new SastToHtmlConverter(
      bundle = scalatags.Text,
      pathManager = pathManager,
      bibliography = Map(),
      sync = None,
      reporter = m => "",
      includeResolver = preprocessed
    ).convertSeq(generatedIndex)(preprocessedCtx)

    pathManager.copyResources(convertedCtx.resourceMap)

    val res = HtmlPages(project.outputdir.relativize(cssfile).toString)
      .wrapContentHtml(convertedCtx.data.toList, "index", HtmlToc.tableOfContents(generatedIndex), "")
    project.outputdir./("index.html").write(res)

    convertedCtx.execTasks()

  }

  private def splitPreprocessed(preprocessedCtxs: List[ConversionContext[(ParsedDocument, Chain[Sast])]])
      : (Map[File, List[Sast]], List[Article]) = {
    preprocessedCtxs.map { ctx =>
      val pd      = ctx.data._1
      val content = ctx.data._2.toList
      (pd.file -> content, Article.articles(pd, content))
    }.unzip.pipe { t => t._1.toMap -> t._2.flatten }
  }

  private def loadKatex(katexmapfile: File): Map[String, String] = {
    Try {
      readFromStream[Map[String, String]](katexmapfile.newInputStream)(mapCodec)
    }.getOrElse(Map())
  }
  private def writeKatex(katexmapfile: File, preprocessedCtx: ConversionContext[Chain[String]]): Any = {
    if (preprocessedCtx.katexMap.nonEmpty) {
      katexmapfile.parent.createDirectories()
      katexmapfile.writeByteArray(writeToArray[Map[String, String]](
        preprocessedCtx.katexMap,
        WriterConfig.withIndentionStep(2)
      )(mapCodec))
    }
  }
  def convertArticle(
      article: Article,
      pathManager: HtmlPathManager,
      project: Project,
      cssfile: File,
      sync: Option[(File, Int)],
      preprocessed: Map[File, List[Sast]],
      preprocessedCtx: ConversionContext[_],
      nlp: Option[NLP]
  ): Unit = {

    val (bibEntries: Seq[Bibliography.BibEntry], biblio) = makeBib(project, article)

    val citations =
      if (bibEntries.isEmpty) Nil
      else {
        import scalatags.Text.all.{id, li, ol}
        import scalatags.Text.short._
        List(ol(bibEntries.zipWithIndex.map { case (be, i) => li(id := be.id, be.format) }))
      }

    val converter = new SastToHtmlConverter(
      bundle = scalatags.Text,
      pathManager = pathManager,
      bibliography = biblio,
      sync = sync,
      reporter = article.sourceDoc.reporter,
      includeResolver = preprocessed
    )
    val toc        = HtmlToc.tableOfContents(article.content)
    val cssrelpath = pathManager.outputDir.relativize(cssfile).toString

    val converted = converter.convertSeq(preprocessed(article.sourceDoc.file))(preprocessedCtx)
    val res = HtmlPages(cssrelpath).wrapContentHtml(
      converted.data.toList ++ citations,
      "fullpost",
      toc,
      article.language
        .orElse(nlp.map(_.language(article.content)))
        .getOrElse("")
    )
    val target = pathManager.translatePost(article.sourceDoc.file)
    target.write(res)
  }

  def preprocess(
      project: Project,
      katexConverter: Option[KatexConverter],
      initialCtx: ConversionContext[Chain[String]]
  )(doc: ParsedDocument): ConversionContext[(ParsedDocument, Chain[Sast])] = {
    val resCtx = new SastToSastConverter(
      project,
      doc.file,
      doc.reporter,
      new ImageConverter(project, preferredFormat = "svg", unsupportedFormat = List("pdf")),
      katexConverter
    ).convertSeq(doc.sast)(initialCtx)
    resCtx.execTasks()
    resCtx.map(doc -> _)
  }

  def makeBib(
      project: Project,
      article: Article
  ): (List[Bibliography.BibEntry], Map[String, String]) = {

    val bibPath =
      article.named.get("bibliography").map { p =>
        article.sourceDoc.file.parent
      }

    val bib = bibPath.toList.flatMap(Bibliography.parse(project.cacheDir))
    val cited = article.analyzed.macros.filter(_.command == Cite)
      .flatMap(_.attributes.positional.flatMap(_.split(",")).map(_.trim)).toSet
    val bibEntries = bib.filter(be => cited.contains(be.id)).sortBy(be => be.authors.map(_.familyName))
    val biblio     = bibEntries.zipWithIndex.map { case (be, i) => be.id -> (i + 1).toString }.toMap
    bibEntries -> biblio
  }
}

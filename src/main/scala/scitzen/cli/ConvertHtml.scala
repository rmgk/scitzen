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
import scitzen.parser.Sast

import scala.util.Try

object ConvertHtml {

  implicit val charset: Charset = StandardCharsets.UTF_8

  // loading ressource statically allows Graal AOT to inline on build
  val stylesheet: Array[Byte] = {
    Resource.asStream("scitzen.css").fold(File("scitzen.css").byteArray)(_.byteArray)
  }

  val mapCodec: JsonValueCodec[Map[String, String]] = JsonCodecMaker.make

  def convertToHtml(project: Project, sync: Option[(File, Int)]): Unit = {

    val unprocessedDocuments = DocumentDirectory(project.root)
    scribe.info(s"found ${unprocessedDocuments.documents.size} documents")

    project.outputdir.createDirectories()

    val cssfile = project.outputdir / "scitzen.css"
    cssfile.writeByteArray(stylesheet)

    val nlp: Option[NLP] =
      if (project.nlpdir.isDirectory) Some(NLP.loadFrom(project.nlpdir, unprocessedDocuments)) else None

    val katexmapfile    = project.cacheDir / "katexmap.json"
    val initialKatexMap = loadKatex(katexmapfile)

    val initialCtx = ConversionContext(Chain.empty[String], katexMap = initialKatexMap)

    import scala.jdk.CollectionConverters._
    val preprocessedCtxs: List[ConversionContext[(Document, Chain[Sast])]] =
      unprocessedDocuments.documents.asJava.parallelStream().map {
        preprocess(project, Some(KatexConverter(Some(katexmapfile))), initialCtx)
      }.iterator().asScala.toList
    val preprocessedDocuments = splitPreprocessed(preprocessedCtxs)
    val preprocessedCtx       = preprocessedCtxs.foldLeft(initialCtx) { case (prev, next) => prev.merge(next) }
    val articles = preprocessedDocuments.documents.flatMap(Article.articles)
      .map { article =>
        val add = Article.recursiveIncludes(article, project, preprocessedDocuments)
        article.copy(includes = add)
      }
    writeKatex(katexmapfile, preprocessedCtx)

    val articleOutput = project.outputdir / "articles"
    articleOutput.createDirectories()
    val pathManager = HtmlPathManager(project.root, project, articleOutput)
    val resources = articles.flatMap { article =>
      val cctx = convertArticle(
        article,
        pathManager.changeWorkingFile(article.sourceDoc.file),
        project,
        cssfile,
        sync,
        preprocessedDocuments,
        preprocessedCtx,
        nlp,
        articles
      )
      cctx.execTasks()
      cctx.resourceMap
    }

    val generatedIndex = GenIndexPage.makeIndex(articles, pathManager, reverse = true)
    val convertedCtx = new SastToHtmlConverter(
      bundle = scalatags.Text,
      pathManager = pathManager,
      bibliography = Map(),
      sync = None,
      reporter = m => "",
      includeResolver = preprocessedDocuments,
      articles = articles
    ).convertSeq(generatedIndex)(preprocessedCtx)

    pathManager.copyResources(resources)

    val res = HtmlPages(project.outputdir.relativize(cssfile).toString)
      .wrapContentHtml(convertedCtx.data.toList, "index", HtmlToc.tableOfContents(generatedIndex), "")
    project.outputdir./("index.html").write(res)

    convertedCtx.execTasks()

  }

  def splitPreprocessed(preprocessedCtxs: List[ConversionContext[(Document, Chain[Sast])]]): DocumentDirectory = {
    DocumentDirectory(preprocessedCtxs.map { ctx =>
      val pd      = ctx.data._1
      val content = ctx.data._2.toList
      pd.copy(sast = content, includes = ctx.includes)
    })
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
      preprocessed: DocumentDirectory,
      preprocessedCtx: ConversionContext[_],
      nlp: Option[NLP],
      articles: List[Article]
  ): ConversionContext[_] = {

    val biblio = makeBib(project, article)

    val converter = new SastToHtmlConverter(
      bundle = scalatags.Text,
      pathManager = pathManager,
      bibliography = biblio,
      sync = sync,
      reporter = article.sourceDoc.reporter,
      includeResolver = preprocessed,
      articles = articles
    )
    val toc        = HtmlToc.tableOfContents(article.content)
    val cssrelpath = pathManager.articleOutputDir.relativize(cssfile).toString

    val convertedArticleCtx = converter.convertSeq(article.content)(preprocessedCtx)
    val headerCtx           = converter.articleHeader(article)(convertedArticleCtx.empty)

    val bibEntries = convertedArticleCtx.usedCitations
    val citations =
      if (bibEntries.isEmpty) Nil
      else {
        import scalatags.Text.all.{id, li, ol, stringAttr, SeqFrag}
        List(ol(bibEntries.map { be => li(id := be.id, be.format) }))
      }

    val res = HtmlPages(cssrelpath).wrapContentHtml(
      headerCtx.data +: convertedArticleCtx.data.toList ++: citations,
      "fullpost",
      toc,
      article.language
        .orElse(nlp.map(_.language(article.content)))
        .getOrElse("")
    )
    val target = pathManager.articleOutputPath(article)
    target.write(res)
    convertedArticleCtx
  }

  def preprocess(
      project: Project,
      katexConverter: Option[KatexConverter],
      initialCtx: ConversionContext[Chain[String]]
  )(doc: Document): ConversionContext[(Document, Chain[Sast])] = {
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

  def makeBib(project: Project, article: Article): Map[String, Bibliography.BibEntry] = {
    article.named.get("bibliography").map { p =>
      val path = article.sourceDoc.file.parent / p
      Bibliography.parse(project.cacheDir)(path)
    }.getOrElse(Nil).sortBy(be => be.authors.map(_.familyName)).zipWithIndex.map {
      case (be, i) => be.id -> be.copy(citekey = Some((i + 1).toString))
    }.toMap
  }
}

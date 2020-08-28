package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.implicits._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scitzen.extern.{Bibliography, ImageConverter, KatexConverter}
import scitzen.generic._
import scitzen.outputs.{GenIndexPage, HtmlPages, HtmlToc, SastToHtmlConverter}

import scala.util.Try

object ConvertHtml {

  implicit val charset: Charset = StandardCharsets.UTF_8

  // loading ressource statically allows Graal AOT to inline on build
  val stylesheet: Array[Byte] = {
    Resource.asStream("scitzen.css").fold(File("scitzen.css").byteArray)(_.byteArray)
  }

  val mapCodec: JsonValueCodec[Map[String, String]] = JsonCodecMaker.make

  def convertToHtml(project: Project, sync: Option[(File, Int)], documentDirectory: DocumentDirectory): Unit = {

    val preprocessed = Common.preprocessDocuments(
      project,
      new ImageConverter(project, preferredFormat = "svg", unsupportedFormat = List("pdf")),
      documentDirectory
    )

    project.outputdir.createDirectories()

    val katexmapfile = project.cacheDir / "katexmap.json"
    val cssfile      = project.outputdir / "scitzen.css"
    cssfile.writeByteArray(stylesheet)

    val nlp: Option[NLP] =
      Option.when(project.nlpdir.isDirectory) {
        NLP.loadFrom(project.nlpdir)
      }

    val pathManager = {
      val articleOutput = project.outputdir / "articles"
      articleOutput.createDirectories()
      HtmlPathManager(project.root, project, articleOutput)
    }

    @scala.annotation.tailrec
    def procRec(
        rem: List[Article],
        katexmap: Map[String, String],
        resourcemap: Map[File, Path]
    ): (Map[String, String], Map[File, Path]) = {
      rem match {
        case Nil => (katexmap, resourcemap)
        case article :: rest =>
          val kc =
            KatexConverter(katexmap, article.named.get("katexMacros").flatMap(project.resolve(project.root, _)))
          val cctx = convertArticle(
            article,
            pathManager.changeWorkingFile(article.sourceDoc.file),
            project,
            cssfile,
            sync,
            preprocessed.directory,
            ConversionContext((), labelledThings = preprocessed.labels, katexConverter = kc),
            nlp,
            preprocessed.articles
          )
          cctx.execTasks()
          procRec(rest, katexmap ++ cctx.katexConverter.cache, resourcemap ++ cctx.resourceMap)
      }
    }

    val (katexRes, resources) = procRec(preprocessed.articles, loadKatex(katexmapfile), Map.empty)
    pathManager.copyResources(resources)
    writeKatex(katexmapfile, katexRes)

    makeindex(project, preprocessed, cssfile, pathManager)
  }

  private def makeindex(
      project: Project,
      preprocessed: Common.PreprocessedResults,
      cssfile: File,
      pathManager: HtmlPathManager
  ): Unit = {
    val generatedIndex = GenIndexPage.makeIndex(preprocessed.articles, pathManager, reverse = true)
    val convertedCtx = new SastToHtmlConverter(
      bundle = scalatags.Text,
      pathManager = pathManager,
      bibliography = Map(),
      sync = None,
      reporter = _ => "",
      includeResolver = preprocessed.directory,
      articles = preprocessed.articles
    ).convertSeq(generatedIndex)(ConversionContext((), labelledThings = preprocessed.labels))

    val res = HtmlPages(project.outputdir.relativize(cssfile).toString)
      .wrapContentHtml(convertedCtx.data.toList, "index", HtmlToc.tableOfContents(convertedCtx.sections.reverse), None)
    project.outputdir./("index.html").write(res)

    convertedCtx.execTasks()
  }

  private def loadKatex(katexmapfile: File): Map[String, String] = {
    Try {
      readFromStream[Map[String, String]](katexmapfile.newInputStream)(mapCodec)
    }.getOrElse(Map())
  }

  private def writeKatex(katexmapfile: File, katexMap: Map[String, String]): Any = {
    if (katexMap.nonEmpty) {
      katexmapfile.parent.createDirectories()
      katexmapfile.writeByteArray(writeToArray[Map[String, String]](
        katexMap,
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
    val cssrelpath = pathManager.articleOutputDir.relativize(cssfile).toString

    val convertedArticleCtx = converter.convertSeq(article.content)(preprocessedCtx)
    val headerCtx           = converter.articleHeader(article)(convertedArticleCtx.empty)

    val bibEntries = convertedArticleCtx.usedCitations.sortBy(_.authors.map(_.familyName)).distinct
    val citations =
      if (bibEntries.isEmpty) Nil
      else {
        import scalatags.Text.all.{SeqFrag, h1, id, li, stringAttr, stringFrag, ul}
        List(h1("Bibliography"), ul(bibEntries.map { be => li(id := be.id, be.format) }))
      }

    val toc = HtmlToc.tableOfContents(convertedArticleCtx.sections.reverse)

    val res = HtmlPages(cssrelpath).wrapContentHtml(
      headerCtx.data +: convertedArticleCtx.data.toList ++: citations,
      "fullpost",
      toc,
      article.language
        .orElse(nlp.flatMap(_.language(article.content)))
    )
    val target = pathManager.articleOutputPath(article)
    target.write(res)
    convertedArticleCtx
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

package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}

import better.files._
import cats.data.Chain
import cats.implicits._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scitzen.extern.{Bibliography, ImageConverter, KatexConverter}
import scitzen.generic._
import scitzen.outputs.{HtmlPages, HtmlToc, SastToHtmlConverter, SastToSastConverter}
import scitzen.parser.MacroCommand.Cite
import scitzen.parser.Sast
import scitzen.parser.Sast.Section
import scala.util.chaining._

import scala.util.Try

object ConvertHtml {

  implicit val charset: Charset = StandardCharsets.UTF_8

  // loading ressource statically allows Graal AOT to inline on build
  val stylesheet: Array[Byte] = {
    Resource.asStream("scitzen.css").fold(File("scitzen.css").byteArray)(_.byteArray)
  }

  val mapCodec: JsonValueCodec[Map[String, String]] = JsonCodecMaker.make

  def convertToHtml(project: Project, sync: Option[(File, Int)]): Unit = {

    val documentManager = project.documentManager
    scribe.info(s"found ${documentManager.documents.size} posts")

    project.outputdir.createDirectories()

    val cssfile = project.outputdir / "scitzen.css"
    cssfile.writeByteArray(stylesheet)

    val nlp = if (project.nlpdir.isDirectory) Some(NLP.loadFrom(project.nlpdir, documentManager)) else None

    val (bibEntries: Seq[Bibliography.BibEntry], biblio) = {
      val doc = documentManager.fulldocs.find(_.analyzed.named.contains("bibliography"))
      val bibPath = doc.flatMap(doc =>
        doc.analyzed.named.get("bibliography").map { p =>
          doc.parsed.file.parent./(p.trim)
        }
      )
      val bib = bibPath.toList.flatMap(Bibliography.parse(project.cacheDir))
      val cited = documentManager.analyzed.flatMap {
        _.analyzeResult.macros.filter(_.command == Cite)
          .flatMap(_.attributes.positional.flatMap(_.split(",")).map(_.trim))
      }.toSet
      val bibEntries = bib.filter(be => cited.contains(be.id)).sortBy(be => be.authors.map(_.familyName))
      val biblio     = bibEntries.zipWithIndex.map { case (be, i) => be.id -> (i + 1).toString }.toMap
      bibEntries -> biblio
    }

    val katexmapfile = project.cacheDir / "katexmap.json"
    val initialKatexMap = Try {
      readFromStream[Map[String, String]](katexmapfile.newInputStream)(mapCodec)
    }.getOrElse(Map())

    def conversionPreproc(doc: ParsedDocument): SastToSastConverter = {
      new SastToSastConverter(
        project,
        doc.file,
        doc.reporter,
        new ImageConverter(project, preferredFormat = "svg", unsupportedFormat = List("pdf")),
        Some(KatexConverter(Some(katexmapfile)))
      )
    }

    val initialCtx = ConversionContext(Chain.empty[String], katexMap = initialKatexMap)

    import scala.jdk.CollectionConverters._
    val preprocessedCtxs: List[ConversionContext[(ParsedDocument, Chain[Sast])]] =
      documentManager.documents.asJava.parallelStream().map { doc =>
        val resCtx = conversionPreproc(doc).convertSeq(doc.sast)(initialCtx)
        resCtx.execTasks()
        resCtx.map(doc -> _)
      }.iterator().asScala.toList

    val (preprocessed, articles)    = {
      preprocessedCtxs.map { ctx =>
        val pd      = ctx.data._1
        val content = ctx.data._2.toList

        def notArticleHeader(sast: Sast) = sast match {
          case Section(title, "=", attributes) => false
          case other => true
        }
        def rec(rem: List[Sast]): List[Article] = {
          rem.dropWhile(notArticleHeader) match {
            case (sec @ Section(title, "=", attributes)) :: rest =>
              val (cont, other) = rest.span(notArticleHeader)
              Article(sec, cont, pd) :: rec(other)
            case other => Nil
          }
        }

        (pd.file -> content, rec(content))
      }.unzip.pipe{t => t._1.toMap -> t._2.flatten}
    }
    val preprocessedCtx = preprocessedCtxs.foldLeft(initialCtx) { case (prev, next) => prev.merge(next) }

    def convertArticle(
        article: Article,
        pathManager: HtmlPathManager
    ): ConversionContext[Map[File, List[Sast]]] = {

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
      val toc        = HtmlToc.tableOfContents(article.content, 2)
      val cssrelpath = pathManager.outputDir.relativize(cssfile).toString

      val converted = converter.convertSeq(preprocessed(article.sourceDoc.file))(preprocessedCtx)
      val res = HtmlPages(cssrelpath).wrapContentHtml(
        converted.data.toList ++ citations,
        "fullpost",
        toc,
        article.language
          //.orElse(nlp.map(_.language(analyzedDoc)))
          .getOrElse("")
      )
      val target = pathManager.translatePost(article.sourceDoc.file)
      target.write(res)
      converted.ret(preprocessed)
    }

    val articleOutput = project.outputdir / "articles"
    articleOutput.createDirectories()

    val pathManager = HtmlPathManager(project.root, project, articleOutput)

    articles.foreach { article =>
      convertArticle(article, pathManager.changeWorkingFile(article.sourceDoc.file))
    }

    val generatedIndex = GenIndexPage.makeIndex(documentManager, project, reverse = true, nlp = nlp)
    val converter = new SastToHtmlConverter(
      bundle = scalatags.Text,
      pathManager = pathManager,
      bibliography = Map(),
      sync = None,
      reporter = m => "",
      includeResolver = preprocessed
    )
    val toc = HtmlToc.tableOfContents(generatedIndex, 2)

    val convertedCtx = converter.convertSeq(generatedIndex)(preprocessedCtx)

    pathManager.copyResources(convertedCtx.resourceMap)

    val res = HtmlPages(project.outputdir.relativize(cssfile).toString)
      .wrapContentHtml(convertedCtx.data.toList, "index", toc, "")
    project.outputdir./("index.html").write(res)

    convertedCtx.execTasks()

    if (convertedCtx.katexMap.nonEmpty) {
      katexmapfile.parent.createDirectories()
      katexmapfile.writeByteArray(writeToArray[Map[String, String]](
        convertedCtx.katexMap,
        WriterConfig.withIndentionStep(2)
      )(mapCodec))
    }

  }

}

package scitzen.cli

import better.files.*
import scitzen.contexts.ConversionContext
import scitzen.extern.Katex.{KatexConverter, KatexLibrary}
import scitzen.generic.*
import scitzen.outputs.{GenIndexPage, HtmlPages, HtmlToc, SastToHtmlConverter}
import com.github.plokhotnyuk.jsoniter_scala.core.*
import scalatags.Text.all.raw
import scalatags.Text.tags2.style
import scitzen.bibliography.Bibtex
import scitzen.cli.ScitzenCommandline.ClSync
import math.Ordering.Implicits.seqOrdering

import scitzen.extern.Katex.mapCodec

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path
import scala.annotation.tailrec
import scala.util.Try

object ConvertHtml:

  implicit val charset: Charset = StandardCharsets.UTF_8

  // loading ressource statically allows Graal AOT to inline on build
  val stylesheet: Array[Byte] =
    Resource.asStream("scitzen.css").fold(File("scitzen.css").byteArray)(_.byteArray)

  def convertToHtml(
      project: Project,
      sync: Option[ClSync],
      preprocessed: PreprocessedResults,
  ): Unit =

    val katexmapfile = project.cacheDir / "katexmap.json"

    val nlp: Option[NLP] =
      Option.when(project.nlpdir.isDirectory) {
        NLP.loadFrom(project.nlpdir)
      }

    val pathManager =
      val articleOutput = project.outputdir / "web"
      articleOutput.createDirectories()
      HtmlPathManager(project.root, project, articleOutput)

    val cssfile   = pathManager.articleOutputDir / "scitzen.css"
    val cssstring = new String(stylesheet, charset)
    cssfile.writeByteArray(stylesheet)

    @tailrec
    def procRec(
        rem: List[Article],
        katexmap: Map[String, String],
        resourcemap: Map[File, Path]
    ): (Map[String, String], Map[File, Path]) =
      rem match
        case Nil => (katexmap, resourcemap)
        case article :: rest =>
          val cctx = convertArticle(
            article,
            pathManager.changeWorkingFile(article.sourceDoc.file),
            cssfile,
            cssstring,
            sync,
            nlp,
            preprocessed,
            KatexConverter(
              katexmap,
              KatexLibrary(article.named.get("katexMacros").flatMap(project.resolve(project.root, _)))
            ),
          )
          procRec(rest, katexmap ++ cctx.katexConverter.cache, resourcemap ++ cctx.resourceMap)

    val (katexRes, resources) = procRec(preprocessed.articles, loadKatex(katexmapfile), Map.empty)
    pathManager.copyResources(resources)
    writeKatex(katexmapfile, katexRes)

    makeindex(project, preprocessed, cssfile, cssstring, pathManager)

  private def makeindex(
      project: Project,
      preprocessed: PreprocessedResults,
      cssfile: File,
      cssstring: String,
      pathManager: HtmlPathManager
  ): Unit =
    val generatedIndex = GenIndexPage.makeIndex(preprocessed.articles, pathManager, pathManager.articleOutputDir)
    val convertedCtx = new SastToHtmlConverter(
      bundle = scalatags.Text,
      pathManager = pathManager,
      sync = None,
      reporter = _ => "",
      preprocessed = preprocessed,
    ).convertSeq(generatedIndex)(ConversionContext(()))

    val res = HtmlPages(pathManager.articleOutputDir.relativize(cssfile).toString, cssstring)
      .wrapContentHtml(
        convertedCtx.data.toList,
        "index",
        None,
        HtmlToc.tableOfContents(convertedCtx.sections.reverse),
        "Index",
        None
      )
    pathManager.articleOutputDir./("index.html").write(res)

  private def loadKatex(katexmapfile: File): Map[String, String] =
    Try {
      readFromStream[Map[String, String]](katexmapfile.newInputStream)
    }.getOrElse(Map())

  private def writeKatex(katexmapfile: File, katexMap: Map[String, String]): Any =
    if katexMap.nonEmpty then
      katexmapfile.parent.createDirectories()
      katexmapfile.writeByteArray(writeToArray[Map[String, String]](
        katexMap,
        WriterConfig.withIndentionStep(2)
      )(mapCodec))

  def convertArticle(
      article: Article,
      pathManager: HtmlPathManager,
      cssfile: File,
      cssstring: String,
      sync: Option[ClSync],
      nlp: Option[NLP],
      preprocessed: PreprocessedResults,
      katexConverter: KatexConverter,
  ): ConversionContext[?] =

    val converter = new SastToHtmlConverter(
      bundle = scalatags.Text,
      pathManager = pathManager,
      sync = sync,
      reporter = article.sourceDoc.reporter,
      preprocessed = preprocessed,
    )
    val cssrelpath = pathManager.articleOutputDir.relativize(cssfile).toString

    val convertedArticleCtx =
      converter.convertSeq(article.content)(ConversionContext((), katexConverter = katexConverter))
    val headerCtx = converter.articleHeader(article)(convertedArticleCtx.empty)

    val bibEntries = convertedArticleCtx.usedCitations.sortBy(_.authors.map(_.familyName)).distinct
    val citations =
      if bibEntries.isEmpty then Nil
      else
        import scalatags.Text.all.{SeqFrag, h2, id, li, stringAttr, stringFrag, ul, cls}
        List(
          h2("Bibliography"),
          ul(cls := "bibliography", bibEntries.map { be => li(id := be.id, be.formatHtmlCitation) })
        )

    val toc = HtmlToc.tableOfContents(convertedArticleCtx.sections.reverse)

    import scalatags.Text.all.{SeqFrag, a, href, stringAttr, stringFrag, Frag, frag, div}

    val res = article.header.attributes.named.get("htmlTemplate") match
      case None =>
        val contentFrag = headerCtx.data +: convertedArticleCtx.data.toList ++: citations

        HtmlPages(cssrelpath, cssstring).wrapContentHtml(
          contentFrag,
          "fullpost",
          if article.named.get("style").contains("article") then None else Some("adhoc"),
          toc.map(c => frag(a(href := s"#${article.header.ref}", article.title): Frag, c: Frag)),
          article.title,
          article.language
            .orElse(nlp.flatMap(_.language(article.content)))
        )
      case Some(templatePath) =>
        val content = SeqFrag(convertedArticleCtx.data.toList).render

        val templateSettings =
          pathManager.project.config.definitions ++ article.header.attributes.named ++ List(
            Some("template content" -> content)
          ).flatten ++ convertedArticleCtx.features.toList.map(s => s"feature $s" -> "")

        ConvertTemplate.fillTemplate(
          pathManager.project,
          preprocessed.directory,
          templatePath,
          templateSettings
        )
    val target = pathManager.articleOutputPath(article)
    target.write(res)
    convertedArticleCtx

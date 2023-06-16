package scitzen.cli

import com.github.plokhotnyuk.jsoniter_scala.core.*
import scitzen.bibliography.BibDB
import scitzen.cli.ScitzenCommandline.ClSync
import scitzen.contexts.ConversionContext
import scitzen.extern.Katex.{KatexConverter, KatexLibrary, mapCodec}
import scitzen.extern.ResourceUtil
import scitzen.generic.*
import scitzen.outputs.{GenIndexPage, HtmlPages, HtmlToc, SastToHtmlConverter}
import scitzen.sast.{Attributes, Prov, Section}

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.math.Ordering.Implicits.seqOrdering
import scala.util.Using

object ConvertHtml:

  implicit val charset: Charset = StandardCharsets.UTF_8

  val stylesheet: Array[Byte] = ResourceUtil.load("scitzen.css")

  def convertToHtml(
      project: Project,
      sync: Option[ClSync],
      preprocessed: PreprocessedResults,
      bibDB: BibDB
  ): Unit =

    val katexmapfile = project.cacheDir.resolve("katexmap.json")

    val nlp: NLP = NLP.loadFromResources

    val pathManager =
      val articleOutput = project.outputdir.resolve("web")
      Files.createDirectories(articleOutput)
      HtmlPathManager(project.root, project, articleOutput)

    val cssfile   = pathManager.articleOutputDir.resolve("scitzen.css")
    val cssstring = new String(stylesheet, charset)
    Files.write(cssfile, stylesheet)

    @tailrec
    def procRec(
        rem: List[Article],
        katexmap: Map[String, String],
        resourcemap: Map[Path, Path]
    ): (Map[String, String], Map[Path, Path]) =
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
            bibDB
          )
          procRec(rest, katexmap ++ cctx.katexConverter.cache, resourcemap ++ cctx.resourceMap)

    val (katexRes, resources) = procRec(preprocessed.articles, loadKatex(katexmapfile), Map.empty)
    pathManager.copyResources(resources)
    writeKatex(katexmapfile, katexRes)

    makeindex(preprocessed, cssfile, cssstring, pathManager)

  private def makeindex(
      preprocessed: PreprocessedResults,
      cssfile: Path,
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
      bibliography = BibDB.empty
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
    Files.writeString(pathManager.articleOutputDir.resolve("index.html"), res)
    ()

  private def loadKatex(katexmapfile: Path): Map[String, String] =
    Using(Files.newInputStream(katexmapfile)) { is => readFromStream[Map[String, String]](is) }.getOrElse(Map())

  private def writeKatex(katexmapfile: Path, katexMap: Map[String, String]): Any =
    if katexMap.nonEmpty then
      Files.createDirectories(katexmapfile.getParent)
      Files.write(
        katexmapfile,
        writeToArray[Map[String, String]](
          katexMap,
          WriterConfig.withIndentionStep(2)
        )(mapCodec)
      )
      ()

  def convertArticle(
      article: Article,
      pathManager: HtmlPathManager,
      cssfile: Path,
      cssstring: String,
      sync: Option[ClSync],
      nlp: NLP,
      preprocessed: PreprocessedResults,
      katexConverter: KatexConverter,
      bibliography: BibDB
  ): ConversionContext[?] =

    val converter = new SastToHtmlConverter(
      bundle = scalatags.Text,
      pathManager = pathManager,
      sync = sync,
      reporter = article.sourceDoc.reporter,
      preprocessed = preprocessed,
      bibliography = bibliography,
    )
    val cssrelpath = pathManager.articleOutputDir.relativize(cssfile).toString

    val convertedArticleCtx =
      converter.convertSeq(article.content)(ConversionContext((), katexConverter = katexConverter))
    val headerCtx = converter.articleHeader(article)(convertedArticleCtx.empty)

    val bibEntries = convertedArticleCtx.usedCitations.sortBy(_.authors.map(_.familyName)).distinct

    val bibname = "Bibliography"
    val bibid   = s"bibliography (gen)"
    val bibsection = Option.when(bibEntries.nonEmpty)(
      Section(
        scitzen.sast.Text(List(scitzen.sast.InlineText(bibname))),
        "==",
        Attributes(Seq(scitzen.sast.Attribute("unique ref", bibid)))
      )(Prov())
    ).toList
    val citations =
      if bibEntries.isEmpty then Nil
      else
        import scalatags.Text.all.{SeqFrag, cls, h2, id, li, stringAttr, stringFrag, ul}
        List(
          h2(bibname, id := bibid),
          ul(cls         := "bibliography", bibEntries.map { be => li(id := be.id, be.formatHtmlCitation) })
        )

    val toc = HtmlToc.tableOfContents(
      convertedArticleCtx.sections.reverse ++ bibsection
    )

    import scalatags.Text.all.{Frag, SeqFrag, a, frag, href, stringAttr, stringFrag}

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
            .orElse(nlp.language(article.content))
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
    Files.writeString(target, res)
    convertedArticleCtx

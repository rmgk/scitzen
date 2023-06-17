package scitzen.cli

import com.github.plokhotnyuk.jsoniter_scala.core.*
import scitzen.bibliography.BibDB
import scitzen.cli.ScitzenCommandline.ClSync
import scitzen.contexts.{ConversionContext, SastContext}
import scitzen.extern.Katex.{KatexConverter, KatexLibrary, mapCodec}
import scitzen.extern.{BlockConversions, ResourceUtil}
import scitzen.generic.*
import scitzen.outputs.{GenIndexPage, HtmlPages, HtmlToc, SastToHtmlConverter}
import scitzen.sast.{Attributes, Prov, Section}

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.math.Ordering.Implicits.seqOrdering
import scala.util.Using

class ConvertHtml(project: Project, blockConversions: BlockConversions):

  implicit val charset: Charset = StandardCharsets.UTF_8

  val stylesheet: Array[Byte] = ResourceUtil.load("scitzen.css")

  def convertToHtml(
      sync: Option[ClSync],
      articles: ArticleDirectory,
      bibDB: BibDB
  ): Unit =

    val katexmapfile = project.cacheDir.resolve("katexmap.json")

    val nlp: NLP = NLP.loadFromResources

    Files.createDirectories(project.outputdirWeb)

    val cssfile   = project.outputdirWeb.resolve("scitzen.css")
    val cssstring = new String(stylesheet, charset)
    Files.write(cssfile, stylesheet)

    @tailrec
    def procRec(
        rem: List[TitledArticle],
        katexmap: Map[String, String],
        resourcemap: Map[ProjectPath, Path]
    ): (Map[String, String], Map[ProjectPath, Path]) =
      rem match
        case Nil => (katexmap, resourcemap)
        case article :: rest =>
          val cctx = convertArticle(
            article,
            cssfile,
            cssstring,
            sync,
            nlp,
            articles,
            KatexConverter(
              katexmap,
              KatexLibrary(article.header.attributes.named.get("katexMacros").flatMap(project.resolve(project.root, _)))
            ),
            bibDB
          )
          procRec(rest, katexmap ++ cctx.katexConverter.cache, resourcemap ++ cctx.resourceMap)

    val (katexRes, resources) = procRec(articles.titled, loadKatex(katexmapfile), Map.empty)
    project.htmlPaths.copyResources(resources)
    writeKatex(katexmapfile, katexRes)

    makeindex(articles, cssfile, cssstring)

  private def makeindex(
      preprocessed: ArticleDirectory,
      cssfile: Path,
      cssstring: String,
  ): Unit =
    val generatedIndex = GenIndexPage.makeIndex(preprocessed.titled, project, project.htmlPaths.articleOutputDir)
    val convertedCtx = new SastToHtmlConverter(
      bundle = scalatags.Text,
      sourceArticle = Article(
        generatedIndex,
        Document(project.resolve(project.cacheDir, "gen-index.scim").get, Array.emptyByteArray),
        SastContext(()),
        Nil
      ),
      preprocessed = preprocessed,
      bibliography = BibDB.empty,
      blockConversions = blockConversions,
    ).convertSeq(generatedIndex)(ConversionContext(()))

    val res = HtmlPages(project.htmlPaths.articleOutputDir.relativize(cssfile).toString, cssstring)
      .wrapContentHtml(
        convertedCtx.data.toList,
        "index",
        None,
        HtmlToc.tableOfContents(convertedCtx.sections.reverse),
        "Index",
        None
      )
    Files.writeString(project.outputdirWeb.resolve("index.html"), res)
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
      article: TitledArticle,
      cssfile: Path,
      cssstring: String,
      sync: Option[ClSync],
      nlp: NLP,
      preprocessed: ArticleDirectory,
      katexConverter: KatexConverter,
      bibliography: BibDB
  ): ConversionContext[?] =

    val converter = new SastToHtmlConverter(
      bundle = scalatags.Text,
      sourceArticle = article.article,
      preprocessed = preprocessed,
      bibliography = bibliography,
      blockConversions = blockConversions,
    )
    val cssrelpath = project.outputdirWeb.relativize(cssfile).toString

    val convertedArticleCtx =
      converter.convertSeq(article.body)(ConversionContext((), katexConverter = katexConverter))
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
          article.header.language
            .orElse(nlp.language(article.body))
        )
      case Some(templatePath) =>
        val content = SeqFrag(convertedArticleCtx.data.toList).render

        val templateSettings =
          project.config.definitions ++ article.header.attributes.named ++ List(
            Some("template content" -> content)
          ).flatten ++ convertedArticleCtx.features.toList.map(s => s"feature $s" -> "")

        ConvertTemplate.fillTemplate(
          project,
          preprocessed,
          templatePath,
          templateSettings
        )
    val target = project.htmlPaths.articleOutputPath(article.header)
    Files.writeString(target, res)
    convertedArticleCtx

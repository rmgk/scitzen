package scitzen.cli

import com.github.plokhotnyuk.jsoniter_scala.core.*
import scalatags.Text.StringFrag
import scitzen.cli.ScitzenCommandline.ClSync
import scitzen.contexts.ConversionContext
import scitzen.extern.Katex.{KatexConverter, KatexLibrary, mapCodec}
import scitzen.extern.ResourceUtil
import scitzen.generic.*
import scitzen.outputs.{HtmlPages, HtmlToc, SastToHtmlConverter}
import scitzen.sast.{Attribute, Attributes, Prov, Section}

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.math.Ordering.Implicits.seqOrdering
import scala.util.Using

class ConvertHtml(anal: ConversionAnalysis):

  def project = anal.project

  implicit val charset: Charset = StandardCharsets.UTF_8

  val stylesheet: Array[Byte] = ResourceUtil.load("scitzen.css")

  def convertToHtml(
      sync: Option[ClSync],
  ): Unit =

    val katexmapfile = project.cacheDir.resolve("katexmap.json")

    val nlp: NLP = NLP.loadFromResources(anal)

    Files.createDirectories(project.outputdirWeb)

    val cssfile = project.outputdirWeb.resolve("scitzen.css")
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
            sync,
            nlp,
            KatexConverter(
              katexmap,
              KatexLibrary(article.header.attributes.plain("katexMacros").flatMap(project.resolve(project.root, _)))
            ),
          )
          procRec(rest, katexmap ++ cctx.katexConverter.cache, resourcemap ++ cctx.resourceMap)

    val (katexRes, resources) = procRec(anal.directory.fullArticles, loadKatex(katexmapfile), Map.empty)
    project.htmlPaths.copyResources(resources)
    writeKatex(katexmapfile, katexRes)
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
      titled: TitledArticle,
      cssfile: Path,
      sync: Option[ClSync],
      nlp: NLP,
      katexConverter: KatexConverter,
  ): ConversionContext[?] =

    val converter = new SastToHtmlConverter(
      articleRef = titled.article.ref,
      anal = anal,
      Attributes(project.config.settings ++ titled.header.attributes.raw)
    )
    val cssrelpath = project.outputdirWeb.relativize(cssfile).toString

    val convertedArticleCtx =
      converter.convertSastSeq(ConversionContext((), katexConverter = katexConverter), titled.article.sast)

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
      convertedArticleCtx.sections.reverse ++ bibsection,
      converter
    )

    import scalatags.Text.all.{Frag, SeqFrag, a, frag, href, stringAttr}

    val res = titled.header.attributes.plain("htmlTemplate") match
      case None =>
        val contentFrag = convertedArticleCtx.data.toList ++: citations

        HtmlPages(cssrelpath).wrapContentHtml(
          contentFrag,
          bodyClass =
            if titled.header.attributes.plain("disable").exists(_.contains("section numbers"))
            then ""
            else "numbered-sections",
          mainClass = if converter.hardNewlines then Some("adhoc") else None,
          sidebar = toc.map(c => frag(a(href := s"#", StringFrag(titled.header.title)): Frag, c: Frag)),
          titled = converter.convertInlinesCombined(ConversionContext(()), titled.header.titleText.inl).data,
          language = titled.header.language
            .orElse(nlp.language(titled.article))
        )
      case Some(templatePath) =>
        val content = SeqFrag(convertedArticleCtx.data.toList).render

        val templateSettings =
          Attributes(project.config.settings ++ titled.header.attributes.raw ++
            convertedArticleCtx.features.map(s => Attribute(s"feature $s", "")) :+
            Attribute("template content", content))

        ConvertTemplate.fillTemplate(
          project,
          anal.directory,
          templatePath,
          templateSettings
        )
    val target = project.htmlPaths.articleOutputPath(titled.header)
    Files.writeString(target, res)
    convertedArticleCtx

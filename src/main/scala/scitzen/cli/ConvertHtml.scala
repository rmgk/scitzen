package scitzen.cli

import com.github.plokhotnyuk.jsoniter_scala.core.*
import de.rmgk.delay
import de.rmgk.logging.Logger
import scitzen.cli.ScitzenCommandline.ClSync
import scitzen.compat.Logging
import scitzen.compat.Logging.given
import scitzen.contexts.ConversionContext
import scitzen.extern.Katex.{KatexConverter, KatexLibrary, mapCodec}
import scitzen.extern.ResourceUtil
import scitzen.generic.*
import scitzen.html.sag.{Recipe, Sag, SagContext}
import scitzen.outputs.{HtmlPages, SastToHtmlConverter}
import scitzen.sast.{Attribute, Attributes, Prov, Section}

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
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

    val katexConverter =
      val katexLibrary = KatexLibrary(project.config.katexMacros.flatMap(project.resolve(project.root, _)))
      KatexConverter(loadKatex(katexmapfile), katexLibrary)

    def procRec(
        rem: List[TitledArticle],
        done: Set[ArticleRef],
    ): Future[Map[ProjectPath, Path]] =
      val futures = rem.map: titled =>
        Future:
          val cctx = convertArticle(
            titled,
            cssfile,
            sync,
            nlp,
            katexConverter
          )
          val found         = cctx.referenced.toSet -- done
          val foundArticles = found.iterator.flatMap(anal.directory.byRef.get).toList
          procRec(
            foundArticles,
            found union done
          ).map(_ ++ cctx.resourceMap)
        .flatten
      Future.foldLeft(futures)(Map.empty)(_ ++ _)

    val selected = anal.directory.fullArticles.iterator.filter: art =>
      anal.selectionPrefixes.exists: sel =>
        art.article.doc.path.absolute.startsWith(sel)
    val sellist = selected.toList
    if sellist.isEmpty then
      Logging.cli.warn("selection is empty", anal.selectionPrefixes)
    val resources = Await.result(procRec(sellist, sellist.iterator.map(_.article.ref).toSet), Duration.Inf)
    project.htmlPaths.copyResources(resources)
    writeKatex(katexmapfile, katexConverter.cache.get())
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
    val citations: Recipe =
      if bibEntries.isEmpty then de.rmgk.delay.Sync(())
      else
        delay.Sync:
          Sag.h2(bibname, id = bibid).run
          Sag.ul(`class` = "bibliography", bibEntries.map { be => Sag.li(id = be.id, be.formatHtmlCitation) }).run

    val toc = HtmlPages.tableOfContents(
      convertedArticleCtx.sections.reverse ++ bibsection,
      converter
    )

    val res = titled.header.attributes.plain("htmlTemplate") match
      case None =>
        val contentFrag =
          Sag.Concat(Sag.Use(convertedArticleCtx.data.toList), citations)

        HtmlPages(cssrelpath).wrapContentHtml(
          contentFrag,
          bodyClass =
            if titled.header.attributes.plain("disable").exists(_.contains("section numbers"))
            then ""
            else "numbered-sections",
          mainClass = if converter.hardNewlines then Some("adhoc") else None,
          sidebar = toc.map(c => Sag.Concat(Sag.a(href = s"#", titled.header.title), c)),
          titled = converter.convertInlinesCombined(ConversionContext(()), titled.header.titleText.inl).data,
          language = titled.header.language
            .orElse(nlp.language(titled.article))
        )
      case Some(templatePath) =>
        val content =
          val sagctx = new SagContext()
          Sag.Concat(convertedArticleCtx.data.view.map(Sag.Use(_)).toSeq: _*).runInContext(sagctx)
          sagctx.resultString

        val templateSettings =
          Attributes(project.config.settings ++ titled.header.attributes.raw ++
            convertedArticleCtx.features.map(s => Attribute(s"feature $s", "")) :+
            Attribute("template content", content))

        ConvertTemplate.fillTemplate(
          project,
          anal.directory,
          titled.article.doc.resolve(templatePath),
          templateSettings
        )
    val target = project.htmlPaths.articleOutputPath(titled.header)
    Files.writeString(target, res)
    convertedArticleCtx

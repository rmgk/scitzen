package scitzen.cli

import de.rmgk.delay
import de.rmgk.logging.Logger
import scitzen.cli.ScitzenCommandline.ClSync
import scitzen.compat.Logging
import scitzen.compat.Logging.given
import scitzen.contexts.ConversionContext
import scitzen.extern.ResourceUtil
import scitzen.generic.*
import scitzen.html.sag.{Recipe, Sag, SagContext}
import scitzen.outputs.{HtmlPages, SastToHtmlConverter}
import scitzen.sast.{Attribute, Attributes, Prov, Section}

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import scala.math.Ordering.Implicits.seqOrdering

class ConvertHtml(anal: ConversionAnalysis):

  def project = anal.project

  implicit val charset: Charset = StandardCharsets.UTF_8

  val stylesheet: Array[Byte] = ResourceUtil.load("scitzen.css")

  def convertToHtml(
      sync: Option[ClSync],
  ): Unit =
    val nlp: NLP = NLP.loadFromResources(anal)

    Files.createDirectories(project.outputdirWeb)

    val cssfile = project.outputdirWeb.resolve("scitzen.css")
    Files.write(cssfile, stylesheet)

    def procRec(
        rem: List[TitledArticle],
        done: Set[ArticleRef],
        acc: Map[ProjectPath, Path]
    ): Map[ProjectPath, Path] =
      rem match
        case Nil => acc
        case titled :: rest =>
          val cctx = convertArticle(
            titled,
            cssfile,
            sync,
            nlp,
          )
          val found = cctx.referenced.toSet -- done

          val foundArticles = found.iterator.flatMap(anal.directory.byRef.get).toList
          procRec(
            foundArticles reverse_::: rest,
            done ++ found,
            acc ++ cctx.resourceMap
          )

    val selected = anal.directory.fullArticles.iterator.filter: art =>
      anal.selectionPrefixes.exists: sel =>
        art.article.doc.path.absolute.startsWith(sel)
    val sellist = selected.toList
    if sellist.isEmpty then
      Logging.cli.warn("selection is empty", anal.selectionPrefixes)

    val resources = procRec(sellist, sellist.map(_.article.ref).toSet, Map.empty)
    project.htmlPaths.copyResources(resources)
    ()

  def convertArticle(
      titled: TitledArticle,
      cssfile: Path,
      sync: Option[ClSync],
      nlp: NLP,
  ): ConversionContext[?] =

    val converter = new SastToHtmlConverter(
      articleRef = titled.article.ref,
      anal = anal,
      Attributes(project.config.settings ++ titled.header.attributes.raw)
    )
    val cssrelpath = project.outputdirWeb.relativize(cssfile).toString

    val convertedArticleCtx =
      converter.convertSastSeq(ConversionContext(()), titled.article.sast)

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
      if bibEntries.isEmpty then Recipe(())
      else
        Recipe:
          Sag.h2(bibname, id = bibid).run
          Sag.ul(`class` = "bibliography", bibEntries.map { be => Sag.li(id = be.id, be.formatHtmlCitation) }).run

    val toc = HtmlPages.tableOfContents(
      convertedArticleCtx.sections.reverse ++ bibsection,
      converter
    )

    val res = titled.header.attributes.plain("htmlTemplate") match
      case None =>
        val contentFrag = Recipe:
          convertedArticleCtx.data.foreach(_.run)
          citations.run

        val mainClass =
          val hardwrap = if converter.hardNewlines then Some("hardwrap") else None
          val noJustify = if converter.combinedAttributes.plainList("flags").contains("-justify")
            then Some("no-justify") else None
          val parts = List(hardwrap, noJustify).flatten
          Option.when(parts.nonEmpty):
            parts.mkString(" ")

        HtmlPages(cssrelpath).wrapContentHtml(
          contentFrag,
          bodyClass =
            if titled.header.attributes.plainList("flags").contains("-section numbers")
            then None
            else Some("numbered-sections"),
          mainClass = mainClass,
          sidebar = toc.map: c =>
            Recipe:
              Sag.a(href = s"#", titled.header.title).run
              c.run
          ,
          titled = converter.convertInlinesCombined(ConversionContext(()), titled.header.titleText.inl).data,
          language = titled.header.language
            .orElse(project.config.defaultLanguage)
        )
      case Some(templatePath) =>
        val content =
          val sagctx = new SagContext()
          convertedArticleCtx.data.view.foreach(_.runInContext(sagctx))
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

package scitzen.cli

import de.rmgk.delay
import scitzen.cli.ScitzenCommandline.ClSync
import scitzen.contexts.{ConversionContext, FileDependency}
import scitzen.extern.ResourceUtil
import scitzen.project.*
import scitzen.html.sag.{Recipe, Sag, SagContext}
import scitzen.outputs.{HtmlPages, SastToHtmlConverter}
import scitzen.sast.{Attribute, Attributes, Prov, Section}

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.math.Ordering.Implicits.seqOrdering

class ConvertHtml(anal: ConversionAnalysis):

  def project = anal.project

  implicit val charset: Charset = StandardCharsets.UTF_8

  val stylesheet: Array[Byte] = ResourceUtil.load("scitzen.css")

  def convertToHtml(
      sync: Option[ClSync],
  ): List[FileDependency] =

    Files.createDirectories(project.outputdirWeb)

    val cssfile = project.outputdirWeb.resolve("scitzen.css")
    Files.write(cssfile, stylesheet)

    @tailrec
    def procRec(
        rem: List[TitledArticle],
        done: Set[ArticleRef],
        acc: List[FileDependency],
    ): List[FileDependency] =
      rem match
        case Nil => acc
        case titled :: rest =>
          val cctx = convertArticle(
            titled,
            cssfile,
            sync,
          )
          val found = cctx.referenced.toSet -- done

          val foundArticles = found.iterator.flatMap(anal.directory.byRef.get).toList
          procRec(
            foundArticles reverse_::: rest,
            done ++ found,
            acc ++ cctx.fileDependencies
          )

    val htmlSelected = anal.selected.filter(ta => ta.flags.html)
    procRec(htmlSelected, htmlSelected.map(_.article.ref).toSet, Nil)

  def convertArticle(
      titled: TitledArticle,
      cssfile: Path,
      sync: Option[ClSync],
  ): ConversionContext[?] =

    val targetPath = project.imagePaths.articleOutputPath(titled.header)

    val converter = new SastToHtmlConverter(
      articleRef = ::(titled.article.ref, Nil),
      anal = anal,
      Attributes(project.config.attrs.raw ++ titled.header.attributes.raw),
      ProjectPath(project, targetPath.getParent)
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
          val hardwrap  = Option.when(titled.flags.hardwrap)("hardwrap")
          val noJustify = Option.when(titled.flags.justify)("justify")
          val parts     = List(hardwrap, noJustify).flatten
          Option.when(parts.nonEmpty):
            parts.mkString(" ")

        HtmlPages(cssrelpath).wrapContentHtml(
          contentFrag,
          bodyClass =
            if titled.flags.`section numbers`
            then Some("numbered-sections")
            else None,
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
          Attributes(project.config.attrs.raw ++ titled.header.attributes.raw ++
            convertedArticleCtx.features.map(s => Attribute(s"feature $s", "")) :+
            Attribute("template content", content))

        ConvertTemplate.fillTemplate(
          project,
          anal.directory,
          titled.article.doc.resolve(templatePath),
          templateSettings
        )
    Files.writeString(targetPath, res)
    convertedArticleCtx

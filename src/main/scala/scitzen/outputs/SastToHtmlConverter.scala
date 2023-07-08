package scitzen.outputs

import de.rmgk.Chain
import de.rmgk.delay.Sync
import scitzen.bibliography.BibEntry
import scitzen.cli.ConversionAnalysis
import scitzen.compat.Logging.cli
import scitzen.contexts.{ConversionContext, FileDependency}
import scitzen.generic.{ArticleRef, ProjectPath, References, SastRef}
import scitzen.html.sag
import scitzen.html.sag.{Recipe, Sag}
import scitzen.images.ImageTarget
import scitzen.sast.Attribute.Named
import scitzen.sast.DCommand.*
import scitzen.sast.*

import java.nio.charset.StandardCharsets
import scala.annotation.unused

class SastToHtmlConverter(
    articleRef: ArticleRef,
    anal: ConversionAnalysis,
    settings: Attributes,
    outputDirectory: ProjectPath
) extends ProtoConverter[Recipe, Recipe](articleRef, anal, settings, outputDirectory):

  override def subconverter(
      articleRef: ArticleRef,
      attr: Attributes
  ): ProtoConverter[Recipe, Recipe] =
    new SastToHtmlConverter(articleRef, anal, attr, outputDirectory)

  override def inlineResToBlock(inl: Chain[Recipe]): Recipe  = Recipe(inl.foreach(_.run))
  override def inlinesAsToplevel(inl: Chain[Recipe]): Recipe = inlineResToBlock(inl)
  override def stringToInlineRes(str: String): Recipe        = Sag.String(str)

  def listItemToHtml(child: ListItem)(ctx: Cta): CtxCF =
    val textCtx = convertInlineSeq(ctx, child.text.inl)
    textCtx.data ++: child.content.fold(textCtx.empty[Recipe])((singleSast: Sast) => convertSast(textCtx, singleSast))

  def categoriesSpan(categories: Seq[String]): Option[Recipe] =
    Option.when(categories.nonEmpty)(
      Sag.span(`class` = "tags", categories.map(c => Sag.String(s" $c ")))
    )

  private val excludedFromMeta =
    Set("label", "categories", "people", "tags", "folder", "date", "flags", "filename", "language")
  def tMeta(ctx: Cta, section: Section): CtxCF =

    @unused val categories = Seq("categories", "people", "tags", "folder").flatMap(section.attributes.plain)
      .flatMap(_.split(","))

    val extraAttributes = ctx.fold[(String, Text), Recipe](section.attributes.raw.collect:
      case Named(id, text) if !id.contains(' ') && !excludedFromMeta.contains(id) => (id, text)
    ):
      case (ctx, (id, text)) =>
        convertInlineSeq(ctx, text.inl).mapc: inner =>
          Sag.span(s"$id = ", inner)

    val metalist =
      section.date.map(date => Sag.time(date.full)) ++
      categoriesSpan(categories) ++
      (if extraAttributes.data.nonEmpty
       then Chain(Sag.table(extraAttributes.data))
       else Chain.empty)

    if metalist.isEmpty then extraAttributes.empty
    else extraAttributes.retc(Sag.div(`class` = "metadata", metalist.toList))

  override def convertSection(ctx: Cta, section: Section): CtxCF =
    if section.attributes.plain("hide").isDefined then return ctx.ret(Chain.empty)
    val Section(title, level, _) = section
    val inlineCtx                = convertInlineSeq(ctx, title.inl)
    val innerFrags               = inlineCtx.data
    val addDepth: Int =
      if level.contains("=") then 0
      else
        ctx.sections.iterator
          .map(_.prefix)
          .find(_.contains("="))
          .fold(1)(s => s.length)
    val link = section.attributes.plain("link").map: url =>
      Sag.a(href = url, "\u2009", Sag.Raw(HtmlPages.iconExternalLink))

    val header = (level.length + addDepth) match
      case 1 => Sag.h1(id = section.ref, innerFrags, link)
      case 2 => Sag.h2(id = section.ref, innerFrags, link)
      case 3 => Sag.h3(id = section.ref, innerFrags, link)
      case 4 => Sag.h4(id = section.ref, innerFrags, link)
      case 5 => Sag.h5(id = section.ref, innerFrags, link)
      case 6 => Sag.h6(id = section.ref, innerFrags, link)
    (header +: tMeta(inlineCtx, section)).push(section)

  override def convertSlist(ctx: Cta, slist: Slist): CtxCF = slist match
    case Slist(Nil) => ctx.empty
    case Slist(children) => children.head.content match
        case None | Some(Slist(_)) =>
          ctx.fold[ListItem, Recipe](children) { (ctx, c) =>
            listItemToHtml(c)(ctx).map(i => Chain(Sag.li(i)))
          }.mapc: i =>
            if children.head.marker.contains(".")
            then Sag.ol(i)
            else Sag.ul(i)
        case _ =>
          ctx.fold[ListItem, Recipe](children) { (ctx, c) =>
            val inlinesCtx = convertInlineSeq(ctx, c.text.inl)
            c.content.fold(inlinesCtx.empty[Recipe])((singleSast: Sast) => convertSast(inlinesCtx, singleSast)).map {
              innerFrags =>
                Chain(Sag.dt(inlinesCtx.data), Sag.dd(innerFrags))
            }
          }.map(i => Chain(Sag.dl(i)))

  override def convertBlockDirective(ctx: Cta, directive: Directive): CtxCF =
    directive.command match

      case Aggregate => handleAggregate(ctx, directive)
      case Index     => handleIndex(ctx, directive)
      case Other("break") =>
        ctx.ret(Chain(Sag.hr()))

      case Ref => handleRef(ctx, directive, produceBlock = true)

      case Include => handleInclude(ctx, directive)

      case other =>
        convertInlineSeq(ctx, List(directive))
  end convertBlockDirective

  def convertBlock(ctx: Cta, block: Block): CtxCF =

    val innerCtx = block.command match
      case BCommand.Other("quote") =>
        val inner = block.content match
          case Parsed(_, content) =>
            convertSastSeq(ctx, content)
          case Fenced(content) =>
            ctx.retc(Sag.String(content))
          case _ => ???
        inner.mapc(i => Sag.blockquote(i))

      case BCommand.Embed =>
        val js = block.content match
          case Fenced(js) => Some(js)
          case _          => None
        ctx.retc(Sag.script(`type` = "text/javascript", js.map(Sag.Raw(_))))

      case other =>
        convertStandardBlock(block, ctx)

    block.attributes.get("note").fold(innerCtx) { note =>
      convertInlineSeq(innerCtx, note.text.inl).map { content =>
        innerCtx.data :+ Sag.p(`class` = "marginnote", content)
      }
    }
  end convertBlock

  def convertStandardBlock(block: Block, ctx: Cta): CtxCF = block.content match
    case Paragraph(text) => convertInlineSeq(ctx, text.inl).map(cf => Chain(Sag.p(cf)))

    case Parsed(delimiter, blockContent) =>
      val label = block.attributes.plain("label")
      if block.command == BCommand.Figure
      then
        blockContent.splitAt(blockContent.size - 1) match
          case (content, Seq(Block(_, _, Paragraph(caption)))) =>
            val contentCtx = convertSastSeq(ctx, content)
            val captionCtx = convertInlinesCombined(contentCtx, caption.inl)
            captionCtx.retc(Sag.figure(id = label, contentCtx.data, Sag.figcaption(captionCtx.data)))
          case other =>
            cli.warn(s"figure needs to end with a paragraph as its caption", block)
            ctx.empty
      else
        convertSastSeq(ctx, blockContent).map { blockContent =>
          if delimiter.isBlank then
            blockContent
          else
            Chain(Sag.section(blockContent, id = label))
        }

    case Fenced(text) => handleCodeListing(ctx, block, text)

    case SpaceComment(_) => ctx.empty
  end convertStandardBlock

  def handleCodeListing(ctx: Cta, block: Block, text: String): CtxCF =
    // Code listing
    // Use this for monospace, space preserving, line preserving text
    // It may wrap to fit the screen content
    val labeltext =
      if block.attributes.get("label").isEmpty then text
      else
        text.replaceAll(""":§([^§]*?)§""", "")

    val language = block.attributes.plain("lang").map(l => s"language-${l}")
    val initTag: Recipe =
      if block.attributes.text.plainString != "highlight" then
        block.attributes.plain("lang") match
          case None       => Sag.code(`class` = language, labeltext)
          case Some(lang) => Sag.code(`class` = language, Sag.Raw(anal.converter.get.convert(lang, labeltext)))
      else
        val lines = labeltext.linesIterator.zipWithIndex.filter { case (s, _) =>
          s.contains(":hl§")
        }.map {
          _._2 + 1
        }.mkString(",")
        val txt = labeltext.replaceAll(""":hl§([^§]*?)§""", "$1")
        Sag.code(txt, `class` = language, `data-line-numbers` = lines)

    val respre = Sag.pre(initTag, id = block.attributes.plain("label"))
    ctx.useFeature("prism").retc(respre)
  end handleCodeListing

  override def convertInlineText(ctx: Cta, inlineText: InlineText): CtxInl = ctx.retc(Sag.String(inlineText.str))
  override def convertInlineDirective(ctx: Cta, directive: Directive): CtxInl =
    val attrs = directive.attributes
    directive.command match
      case Strong => convertInlineSeq(ctx, attrs.text.inl).map(c => Sag.strong(c)).single
      case Emph   => convertInlineSeq(ctx, attrs.text.inl).map(c => Sag.em(c)).single
      case Code   => ctx.retc(Sag.code(attrs.target))
      case Script =>
        doc.resolve(attrs.target) match
          case Some(path) =>
            val rel = project.htmlPaths.relativizeImage(path)
            ctx.retc(Sag.script(`type` = "text/javascript", src = rel.toString))
              .requireInOutput(FileDependency(path, path, rel, outputDirectory))
          case None =>
            cli.warn("no script", directive)
            ctx.retc(stringToInlineRes(directiveString(directive)))

      case Def | Comment => ctx.empty
      case Include | Index | Aggregate =>
        cli.warn("cannot inline", directive)
        ctx.retc(Sag.code(directiveString(directive)))

      case Raw => ctx.retc(Sag.div(Sag.Raw(attrs.plain("html").getOrElse(""))))

      case Math =>
        val inner = attrs.target
        ctx.retc(Sag.math(Sag.Raw(anal.converter.get.convert("katex", inner))))

      case BibQuery =>
        convertInlineDirective(ctx, anal.bib.convert(directive))

      case Cite =>
        handleCite(ctx, directive)

      case Link =>
        val target = attrs.target
        convertInlineSeq(ctx, attrs.text.inl).mapc: content =>
          Sag.a(href = target, content)

      case Ref =>
        handleRef(ctx, directive, produceBlock = false)

      case Lookup =>
        handleLookup(directive) match
          case Some(res) => convertInlineSeq(ctx, res.inl)
          case None =>
            ctx.retc(Sag.code(attrs.target))

      case Other(otherCommand) =>
        otherCommand match
          case "footnote" =>
            convertInlineSeq(ctx, attrs.text.inl).mapc: res =>
              Sag.details(Sag.summary("※"), res)

          case "ins" =>
            convertInlineSeq(ctx, attrs.text.inl).mapc: res =>
              Sag.applyDynamic("ins")(res)
          case "del" =>
            convertInlineSeq(ctx, attrs.text.inl).mapc: res =>
              Sag.applyDynamic("del")(res)

          case "todo" => ctx.retc(Sag.code(`class` = "todo", SastToScimConverter(anal.bib).macroToScim(directive)))
          case "tableofcontents" => ctx.empty
          case "partition"       => ctx.empty
          case "rule"            => ctx.retc(Sag.span(attrs.target, `class` = "rule"))

          case _ =>
            cli.warn("unknown directive", directive)
            ctx.retc(stringToInlineRes(directiveString(directive)))

      case Image => convertImage(ctx, directive)
  end convertInlineDirective

  private def handleRef(ctx: Cta, directive: Directive, produceBlock: Boolean): ConversionContext[Chain[Recipe]] = {
    val attrs: Attributes = directive.attributes
    val candidates        = References.resolve(directive, doc, anal.directory)

    if candidates.sizeIs > 1 then
      cli.warn(
        s"multiple resolutions for ${attrs.target}",
        reporter(directive.prov)
      )
      cli.warn(s"\tresolutions are in: ${candidates.map(c => c.scope).mkString("\n\t", "\n\t", "\n\t")}")

    candidates.headOption.map[CtxCF] { (targetDocument: SastRef) =>
      val nameOpt = attrs.textOption
      val titled  = anal.directory.byRef(targetDocument.articleRef)
      val fileRef =
        if anal.directory.includedInFixpoint.getOrElse(targetDocument.articleRef, Set.empty).contains(articleRef)
        then ""
        else project.htmlPaths.relativeArticleTarget(titled.header).toString

      val resctx = targetDocument.sast match
        case sec @ Section(title, _, _) =>
          convertInlineSeq(ctx, nameOpt.getOrElse(title).inl).map: titleText =>
            val link = Sag.a(href = s"$fileRef#${References.getLabel(targetDocument).get}", titleText)
            if !produceBlock
            then Chain(link)
            else {
              def categories =
                sec.attributes.plain("tags").iterator.flatMap(_.split(",")).map(_.trim).filter(!_.isBlank).toList

              def timeShort(date: Option[String]) = Sag.time(f"${date.getOrElse("")}%-10s")

              Chain(Sag.article(
                timeShort(attrs.plain("datetime").orElse(sec.date.map(_.date.full))),
                " ", // whitespace prevents elements from hugging without stylesheet
                link,
                " ",
                categoriesSpan(categories)
              ))
            }
        case Block(_, attr, _) =>
          val label = References.getLabel(targetDocument).get
          convertInlineSeq(ctx, nameOpt.map(_.inl).getOrElse(Nil)).mapc: titleText =>
            if titleText.isEmpty
            then Sag.a(href = s"$fileRef#$label", label)
            else Sag.a(href = s"$fileRef#$label", titleText, " ", label)

        case other =>
          cli.warn(s"can not refer to $other")
          ctx.empty
      if fileRef.nonEmpty
      then resctx.reference(titled.article.ref)
      else resctx
    }.getOrElse {
      cli.warn(s"no ref resolutions", directive)
      ctx.retc(Sag.code(SastToScimConverter(anal.bib).macroToScim(directive)))
    }
  }

  private def handleCite(ctx: Cta, directive: Directive): CtxInl = {
    val attrs: Attributes = directive.attributes
    val citations         = anal.bib.bibkeys(directive).map(k => k -> anal.bib.entries.get(k))
    val anchors = citations.sortBy(_._2.map(_.citekey)).flatMap {
      case (bibid, Some(bib)) => List(Sag.a(href = s"#$bibid", bib.citekey), Sag.String(",\u2009"))
      case (bibid, None) =>
        cli.warn(s"bib key not found: »${bibid}«", directive)
        List(Sag.code(bibid), Sag.String(" "))
    }.dropRight(1)
    val cctx          = ctx.cite(citations.flatMap(_._2))
    val styledAnchors = Sag.span(`class` = "citations", "(", anchors, ")")
    if attrs.raw.sizeIs > 1 then
      attrs.textOption match
        case None => cctx.retc(styledAnchors)
        case Some(text) =>
          convertInlineSeq(cctx, text.inl) :++ Chain(Sag.String("\u2009"), styledAnchors)
    else if attrs.plain("style").contains("author")
    then
      val nameOption =
        for
          first  <- citations.headOption
          bi     <- first._2
          author <- bi.authors.headOption
          family <- author.familyName
        yield Sag.String(s"${family}${
            if bi.authors.sizeIs > 1 then " et al.\u2009"
            else "\u2009"
          }")
      cctx.ret(nameOption ++: Chain(styledAnchors))
    else cctx.retc(styledAnchors)
  }

  private def convertImage(ctx: Cta, directive: Directive): Ctx[Chain[Recipe]] = {
    convertImage(ctx, directive, ImageTarget.Html): ctx =>
      val path = ctx.data.relativeFinalization
      ctx.retc:
        val filename  = path.getFileName.toString
        val sizeclass = directive.attributes.plain("size").map(s => s"sizing-$s")
        if videoEndings.exists(filename.endsWith) then
          Sag.video(src = path.toString, loop = true, autoplay = true, `class` = sizeclass)
        else
          val myStyle: Option[String] =
            if directive.attributes.target.endsWith(".pdf")
            then Some(s"background-color: white; ${directive.attributes.plain("css_style").getOrElse("")}")
            else directive.attributes.plain("css_style")
          Sag.img(src = path.toString, `class` = sizeclass, style = myStyle)
  }

end SastToHtmlConverter

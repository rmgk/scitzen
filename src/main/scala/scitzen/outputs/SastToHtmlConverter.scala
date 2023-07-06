package scitzen.outputs

import de.rmgk.Chain
import de.rmgk.delay.Sync
import scitzen.bibliography.BibEntry
import scitzen.cli.ConversionAnalysis
import scitzen.compat.Logging.cli
import scitzen.contexts.ConversionContext
import scitzen.extern.ImageTarget
import scitzen.generic.{ArticleRef, Document, References, SastRef}
import scitzen.html.sag
import scitzen.html.sag.{Recipe, Sag}
import scitzen.sast.Attribute.Named
import scitzen.sast.DCommand.*
import scitzen.sast.*

import java.nio.file.Files

class SastToHtmlConverter(
    articleRef: ArticleRef,
    anal: ConversionAnalysis,
    combinedAttributes: Attributes,
) extends ProtoConverter[Recipe, Recipe](articleRef, anal, combinedAttributes):

  override def subconverter(
      articleRef: ArticleRef,
      analysis: ConversionAnalysis,
      attr: Attributes
  ): ProtoConverter[Recipe, Recipe] =
    new SastToHtmlConverter(articleRef, analysis, attr)

  override def inlineResToBlock(inl: Chain[Recipe]): Recipe  = Sag.Chain(inl)
  override def inlinesAsToplevel(inl: Chain[Recipe]): Recipe = inlineResToBlock(inl)
  override def stringToInlineRes(str: String): Recipe        = Sag.String(str)

  def listItemToHtml(child: ListItem)(ctx: Cta): CtxCF =
    val textCtx = convertInlineSeq(ctx, child.text.inl)
    textCtx.data ++: child.content.fold(textCtx.empty[Recipe])((singleSast: Sast) => convertSast(textCtx, singleSast))

  def categoriesSpan(categories: Seq[String]): Option[Recipe] =
    Option.when(categories.nonEmpty)(
      Sag.span(`class` = "category", categories.map(c => Sag.String(s" $c ")))
    )

  def tMeta(article: Section): Recipe =

    def timeFull(date: ScitzenDateTime): Recipe = Sag.time(date.full)

    val categories = List("categories", "people").flatMap(article.attributes.plain)
      .flatMap(_.split(","))

    val metalist =
      article.date.map(timeFull) ++
      categoriesSpan(categories) ++
      article.attributes.plain("folder").map(f => Sag.span(`class` = "category", s" in $f"))

    if metalist.nonEmpty then Sag.div(`class` = "metadata", metalist.toList) else Sag.Nothing

  override def convertSection(ctx: Cta, section: Section): CtxCF =
    if section.attributes.plain("disable").exists(_.contains("display")) then return ctx.ret(Chain.empty)
    val Section(title, level, _) = section
    convertInlineSeq(ctx, title.inl).map { innerFrags =>
      val addDepth: Int =
        if level.contains("=") then 0
        else
          ctx.sections.iterator
            .map(_.prefix)
            .find(_.contains("="))
            .fold(1)(s => s.length)
      val link = section.attributes.plain("link").map: url =>
        Sag.a(href = url, "\u2009", Sag.Raw(HtmlPages.iconExternalLink))

      Chain[Recipe](
        Sync:
          sag.write(s"<h${level.length + addDepth} id=\"")
          Sag.String(section.ref).run
          sag.write("\">")
          innerFrags.foreach(_.run)
          link.foreach(_.run)
          sag.write(s"</h${level.length + addDepth}>")
        ,
        if level == "="
        then tMeta(section)
        else Sag.Nothing
      )
    }.push(section)

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
    val attributes = directive.attributes
    directive.command match

      case Aggregate => handleAggregate(ctx, directive)
      case Index     => handleIndex(ctx, directive)
      case Other("break") =>
        ctx.ret(Chain(Sag.hr()))

      case Other("article") =>
        def timeShort(date: Option[String]) = Sag.time(f"${date.getOrElse("")}%-8s")

        convertInlineSeq(ctx, attributes.text.inl).mapc: text =>
          Sag.article(
            timeShort(attributes.plain("datetime")),
            " ", // whitespace prevents elements from hugging without stylesheet
            Sag.a(
              `class` = "title",
              href = attributes.target,
              text
            ),
            " ",
            categoriesSpan(attributes.raw.collect { case Named("category", value) => value.plainString })
          )

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
              .requireInOutput(path, rel)
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
        handleRef(ctx, directive)

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

  private def handleRef(ctx: Cta, directive: Directive): ConversionContext[Chain[Recipe]] = {
    val attrs: Attributes = directive.attributes
    val scope =
      attrs.plain("scope").flatMap(doc.resolve).getOrElse(doc.path)
    val candidates = References.filterCandidates(scope, anal.directory.labels.getOrElse(attrs.target, Nil))

    if candidates.sizeIs > 1 then
      cli.warn(
        s"multiple resolutions for ${attrs.target}",
        reporter(directive.prov)
      )
      cli.warn(s"\tresolutions are in: ${candidates.map(c => c.scope).mkString("\n\t", "\n\t", "\n\t")}")

    candidates.headOption.map[CtxCF] { (targetDocument: SastRef) =>
      val nameOpt   = attrs.textOption
      val titledOpt = anal.directory.byRef.get(targetDocument.articleRef)
      val fileRef =
        titledOpt match
          case Some(titled) =>
            project.htmlPaths.relativeArticleTarget(titled.header).toString
          case _ => ""

      val resctx = targetDocument.sast match
        case sec @ Section(title, _, _) =>
          convertInlineSeq(ctx, nameOpt.getOrElse(title).inl).map: titleText =>
            Chain(Sag.a(href = s"$fileRef#${sec.ref}", titleText))
        case Block(_, attr, _) =>
          val label = attr.plain("label").get
          convertInlineSeq(ctx, nameOpt.map(_.inl).getOrElse(Nil)).mapc: titleText =>
            if titleText.isEmpty
            then Sag.a(href = s"$fileRef#$label", label)
            else Sag.a(href = s"$fileRef#$label", titleText, " ", label)

        case other =>
          cli.warn(s"can not refer to $other")
          ctx.empty
      resctx.reference(titledOpt.map(_.article.ref).toList)
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
          convertInlineSeq(cctx, text.inl) :++ Chain(Sag.String("\u2009"),  styledAnchors)
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

  private def convertImage(ctx: Cta, mcro: Directive): Ctx[Chain[Recipe]] = {
    val attrs  = mcro.attributes
    val target = anal.image.lookup(doc.resolve(attrs.target).get, ImageTarget.Html)
    if Files.exists(target.absolute)
    then
      val path = project.htmlPaths.relativizeImage(target)
      ctx.requireInOutput(target, path).retc {
        val filename  = path.getFileName.toString
        val sizeclass = mcro.attributes.plain("size").map(s => s"sizing-$s")
        if videoEndings.exists(filename.endsWith) then
          Sag.video(src = path.toString, loop = true, autoplay = true, `class` = sizeclass)
        else
          val myStyle: Option[String] =
            if attrs.target.endsWith(".pdf")
            then Some(s"background-color: white; ${attrs.plain("css_style").getOrElse("")}")
            else attrs.plain("css_style")
          Sag.img(src = path.toString, `class` = sizeclass, style = myStyle)
      }
    else
      cli.warn(s"could not find path ${target}" + reporter(mcro))
      ctx.empty
  }

end SastToHtmlConverter

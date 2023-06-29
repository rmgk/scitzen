package scitzen.outputs

import de.rmgk.Chain
import scalatags.{Text, text}
import scalatags.Text.StringFrag
import scitzen.bibliography.BibEntry
import scitzen.cli.ConversionAnalysis
import scitzen.compat.Logging.cli
import scitzen.extern.{ImageTarget, Prism}
import scitzen.generic.{Document, References, SastRef}
import scitzen.sast.{BCommand, *}
import scitzen.sast.DCommand.*
import scalatags.Text.all.*
import scalatags.Text.tags2
import scalatags.Text.tags2.{math, section, time}
import scalatags.text.Builder
import scitzen.sast.Attribute.Named

import java.nio.file.Files

given [A](using ev: A => Frag): Conversion[Chain[A], Frag] with {
  override def apply(xs: Chain[A]): Frag = new Frag {
    def render                   = xs.map(elem => ev(elem).render).mkString
    def applyTo(t: text.Builder) = xs.foreach(elem => ev(elem).applyTo(t))
  }
}

class SastToHtmlConverter(
    doc: Document,
    anal: ConversionAnalysis,
    combinedAttributes: Attributes,
) extends ProtoConverter[Frag, Frag](doc, anal, combinedAttributes):

  override def subconverter(
      doc: Document,
      analysis: ConversionAnalysis,
      attr: Attributes
  ): ProtoConverter[Text.all.Frag, Text.all.Frag] =
    new SastToHtmlConverter(doc, analysis, attr)

  val syncPos: Int =
//    if sync.exists(_.path == pathManager.cwf) then sync.get._2
//    else
    Int.MaxValue

  override def inlineResToBlock(inl: Chain[Text.all.Frag]): Text.all.Frag  = inl.convert
  override def inlinesAsToplevel(inl: Chain[Text.all.Frag]): Text.all.Frag = inlineResToBlock(inl)
  override def stringToInlineRes(str: String): Text.all.Frag               = stringFrag(str)

  def listItemToHtml(child: ListItem)(ctx: Cta): CtxCF =
    val textCtx = convertInlineSeq(ctx, child.text.inl)
    textCtx.data ++: child.content.fold(textCtx.empty[Frag])((singleSast: Sast) => convertSast(textCtx, singleSast))

  def categoriesSpan(categories: Seq[String]): Option[Tag] =
    Option.when(categories.nonEmpty)(
      span(cls := "category")(categories.map(c => stringFrag(s" $c "))*)
    )

  def tMeta(article: Section): Frag =

    def timeFull(date: ScitzenDateTime): Tag = time(date.full)

    val categories = List("categories", "people").flatMap(article.attributes.plain)
      .flatMap(_.split(","))

    val metalist =
      article.date.map(timeFull) ++
      categoriesSpan(categories) ++
      article.attributes.plain("folder").map(f => span(cls := "category")(stringFrag(s" in $f")))

    if metalist.nonEmpty then div(cls := "metadata")(metalist.toSeq*) else frag()

  override def convertSection(ctx: Cta, section: Section): CtxCF =
    val Section(title, level, _) = section
    convertInlineSeq(ctx, title.inl).map { innerFrags =>
      val addDepth: Int =
        if level.contains("=") then 0
        else
          ctx.sections.iterator
            .map(_.prefix)
            .find(_.contains("="))
            .fold(1)(s => s.length)
      Chain[Frag](
        tag(s"h${level.length + addDepth}")(id := section.ref, innerFrags.convert),
        if level == "="
        then tMeta(section)
        else frag()
      )
    }.push(section)

  override def convertSlist(ctx: Cta, slist: Slist): CtxCF = slist match
    case Slist(Nil) => ctx.empty
    case Slist(children) => children.head.content match
        case None | Some(Slist(_)) =>
          val listTag = if children.head.marker.contains(".") then ol
          else ul
          ctx.fold[ListItem, Frag](children) { (ctx, c) =>
            listItemToHtml(c)(ctx).map(i => Chain(li(i.convert)))
          }.map(i => Chain(listTag(i.convert)))
        case _ =>
          ctx.fold[ListItem, Frag](children) { (ctx, c) =>
            val inlinesCtx = convertInlineSeq(ctx, c.text.inl)
            c.content.fold(inlinesCtx.empty[Frag])((singleSast: Sast) => convertSast(inlinesCtx, singleSast)).map {
              innerFrags =>
                Chain(dt(inlinesCtx.data.convert), dd(innerFrags.convert))
            }
          }.map(i => Chain(dl(i.convert)))

  override def convertBlockDirective(ctx: Cta, directive: Directive): CtxCF =
    val attributes = directive.attributes
    directive.command match

      case Aggregate => handleAggregate(ctx, directive)
      case Index     => handleIndex(ctx, directive)
      case Other("break") =>
        ctx.ret(Chain(hr))

      case Other("article") =>
        def timeShort(date: Option[String]) = time(f"${date.getOrElse("")}%-8s")

        convertInlineSeq(ctx, attributes.text.inl).mapc: text =>
          Chain(tags2.article(
            timeShort(attributes.plain("datetime")),
            " ", // whitespace prevents elements from hugging without stylesheet
            a(
              cls  := "title",
              href := attributes.target,
              text
            ),
            " ",
            categoriesSpan(attributes.raw.collect { case Named("category", value) => value.plainString })
          ))

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
            ctx.retc(stringFrag(content))
          case _ => ???
        inner.mapc(i => blockquote(i.convert))

      case BCommand.Embed =>
        val js = block.content match
          case Fenced(js) => Some(js)
          case _          => None
        ctx.retc(script(`type` := "text/javascript", js.map(raw(_))))

      case other =>
        convertStandardBlock(block, ctx)

    block.attributes.get("note").fold(innerCtx) { note =>
      convertInlineSeq(innerCtx, note.text.inl).map { content =>
        innerCtx.data :+ p(`class` := "marginnote", content.convert)
      }
    }
  end convertBlock

  def convertStandardBlock(block: Block, ctx: Cta): CtxCF = block.content match
    case Paragraph(text) => convertInlineSeq(ctx, text.inl).map(cf => Chain(p(cf.convert)))

    case Parsed(delimiter, blockContent) =>
      val label = block.attributes.plain("label").map(id := _)
      if block.command == BCommand.Figure
      then
        blockContent.splitAt(blockContent.size - 1) match
          case (content, Seq(Block(_, _, Paragraph(caption)))) =>
            val contentCtx = convertSastSeq(ctx, content)
            val captionCtx = convertInlinesCombined(contentCtx, caption.inl)
            captionCtx.retc(figure(label, contentCtx.data, figcaption(captionCtx.data)))
          case other =>
            cli.warn(s"figure needs to end with a paragraph as its caption", block)
            ctx.empty
      else
        convertSastSeq(ctx, blockContent).map { blockContent =>
          if delimiter.isBlank then
            blockContent
          else
            Chain(section(blockContent.convert, label))
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
    val initTag: Tag =
      if block.attributes.text.plainString != "highlight" then
        block.attributes.plain("lang") match
          case None       => code(labeltext)
          case Some(lang) => code(raw(Prism.highlight(labeltext, lang)))
      else
        val lines = labeltext.linesIterator.zipWithIndex.filter { case (s, _) =>
          s.contains(":hl§")
        }.map {
          _._2 + 1
        }.mkString(",")
        val txt = labeltext.replaceAll(""":hl§([^§]*?)§""", "$1")
        code(txt, attr("data-line-numbers") := lines)

    val respre =
      block.attributes.plain("lang").fold(pre(initTag))(l => pre(initTag(cls := s"language-${l}")))
    val res = block.attributes.plain("label").fold(respre: Tag)(l => respre(id := l))
    ctx.useFeature("prism").retc(res)
  end handleCodeListing

  override def convertInlineText(ctx: Cta, inlineText: InlineText): CtxInl = ctx.retc(stringFrag(inlineText.str))
  override def convertInlineDirective(ctx: Cta, directive: Directive): CtxInl =
    val attrs = directive.attributes
    directive.command match
      case Strong => convertInlineSeq(ctx, attrs.text.inl).map(c => strong(c.convert)).single
      case Emph   => convertInlineSeq(ctx, attrs.text.inl).map(c => em(c.convert)).single
      case Code   => ctx.retc(code(attrs.target))
      case Script =>
        doc.resolve(attrs.target) match
          case Some(path) =>
            val rel = project.htmlPaths.relativizeImage(path)
            ctx.retc(script(`type` := "text/javascript", src := rel.toString))
              .requireInOutput(path, rel)
          case None =>
            cli.warn("no script", directive)
            ctx.retc(stringToInlineRes(directiveString(directive)))

      case Def | Comment => ctx.empty
      case Include | Index | Aggregate =>
        cli.warn("cannot inline", directive)
        ctx.retc(code(directiveString(directive)))

      case Raw => ctx.retc(div(raw(attrs.plain("html").getOrElse(""))))

      case Math =>
        val inner = attrs.target
        ctx.katex(inner).map(res => Chain(math(raw(res))))

      case BibQuery =>
        convertInlineDirective(ctx, anal.bib.convert(directive))

      case Cite =>
        handleCite(ctx, directive)

      case Link =>
        val target = attrs.target
        convertInlineSeq(ctx, attrs.text.inl).mapc: content =>
          a(href := target).apply(content.convert)

      case Ref =>
        handleRef(ctx, directive)

      case Lookup =>
        handleLookup(directive) match
          case Some(res) => convertInlineSeq(ctx, res.inl)
          case None =>
            ctx.retc(code(attrs.target))

      case Other(otherCommand) =>
        otherCommand match
          case "footnote" =>
            convertInlineSeq(ctx, attrs.text.inl).mapc: res =>
              tags2.details(tags2.summary("※"), res.convert)

          case tagname @ ("ins" | "del") =>
            convertInlineSeq(ctx, attrs.text.inl).mapc: res =>
              tag(tagname)(res)

          case "todo" => ctx.retc(code(`class` := "todo", SastToScimConverter(anal.bib).macroToScim(directive)))
          case "tableofcontents" => ctx.empty
          case "partition"       => ctx.empty
          case "rule"            => ctx.retc(span(attrs.target, `class` := "rule"))

          case _ =>
            cli.warn("unknown directive", directive)
            ctx.retc(stringToInlineRes(directiveString(directive)))

      case Image => convertImage(ctx, directive)
  end convertInlineDirective

  private def handleRef(ctx: Cta, directive: Directive) = {
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
      val nameOpt    = attrs.textOption
      val articleOpt = targetDocument.directArticle
      val fileRef =
        articleOpt match
          case Some(article) =>
            project.htmlPaths.relativeArticleTarget(article).toString
          case _ => ""

      targetDocument.sast match
        case sec @ Section(title, _, _) =>
          convertInlineSeq(ctx, nameOpt.getOrElse(title).inl).map: titleText =>
            Chain(a(href := s"$fileRef#${sec.ref}", titleText.convert))
        case Block(_, attr, _) =>
          val label = attr.plain("label").get
          convertInlineSeq(ctx, nameOpt.map(_.inl).getOrElse(Nil)).mapc: titleText =>
            if titleText.isEmpty
            then a(href := s"$fileRef#$label", label)
            else a(href := s"$fileRef#$label", titleText.convert, " ", label)

        case other =>
          cli.warn(s"can not refer to $other")
          ctx.empty
    }.getOrElse {
      cli.warn(s"no ref resolutions", directive)
      ctx.retc(code(SastToScimConverter(anal.bib).macroToScim(directive)))
    }
  }

  private def handleCite(ctx: Cta, directive: Directive): CtxInl = {
    val attrs: Attributes = directive.attributes
    val citations         = anal.bib.bibkeys(directive).map(k => k -> anal.bib.entries.get(k))
    val anchors = citations.sortBy(_._2.map(_.citekey)).flatMap {
      case (bibid, Some(bib)) => List(a(href := s"#$bibid", bib.citekey), stringFrag(",\u2009"))
      case (bibid, None) =>
        cli.warn(s"bib key not found: »${bibid}«", directive)
        List(code(bibid), stringFrag(" "))
    }.dropRight(1)
    val cctx          = ctx.cite(citations.flatMap(_._2))
    val styledAnchors = span(cls := "citations", "(", anchors, ")")
    if attrs.raw.sizeIs > 1 then
      convertInlineSeq(cctx, attrs.textOption.getOrElse(scitzen.sast.Text.empty).inl).map { res =>
        if res.isEmpty then Chain(styledAnchors)
        else
          val last = res.last
          val init = res.init
          val addSpace = last match
            case StringFrag(s) => stringFrag(s"$s\u2009")
            case other         => last
          init ++ Chain(addSpace, styledAnchors)
      }
    else if attrs.plain("style").contains("author")
    then
      val nameOption =
        for
          first  <- citations.headOption
          bi     <- first._2
          author <- bi.authors.headOption
          family <- author.familyName
        yield stringFrag(s"${family}${
            if bi.authors.sizeIs > 1 then " et al.\u2009"
            else "\u2009"
          }")
      cctx.ret(nameOption ++: Chain(styledAnchors))
    else cctx.retc(styledAnchors)
  }

  private def convertImage(ctx: Cta, mcro: Directive): Ctx[Chain[Tag]] = {
    val attrs  = mcro.attributes
    val target = anal.image.lookup(doc.resolve(attrs.target).get, ImageTarget.Html)
    if Files.exists(target.absolute)
    then
      val path = project.htmlPaths.relativizeImage(target)
      ctx.requireInOutput(target, path).retc {
        val filename  = path.getFileName.toString
        val sizeclass = mcro.attributes.plain("size").map(s => cls := s"sizing-$s")
        if videoEndings.exists(filename.endsWith) then
          video(src := path.toString, attr("loop").empty, attr("autoplay").empty, sizeclass)
        else
          val myStyle: Modifier =
            if attrs.target.endsWith(".pdf")
            then style := s"background-color: white; ${attrs.plain("css_style").getOrElse("")}"
            else attrs.plain("css_style").map(style := _)
          img(src := path.toString, sizeclass, myStyle)
      }
    else
      cli.warn(s"could not find path ${target}" + reporter(mcro))
      ctx.empty
  }

end SastToHtmlConverter

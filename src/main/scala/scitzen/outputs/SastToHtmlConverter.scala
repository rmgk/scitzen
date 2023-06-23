package scitzen.outputs

import de.rmgk.Chain
import scalatags.{Text, text}
import scalatags.Text.StringFrag
import scitzen.bibliography.BibEntry
import scitzen.cli.ConversionAnalysis
import scitzen.compat.Logging.scribe
import scitzen.extern.{ImageTarget, Prism}
import scitzen.generic.{Document, References, SastRef, TitledArticle}
import scitzen.sast.*
import scitzen.sast.Attribute.Plain
import scitzen.sast.DCommand.*
import scalatags.Text.all.*
import scalatags.Text.tags2
import scalatags.Text.tags2.{math, section, time}
import scalatags.text.Builder

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
    val textCtx = convertInlineSeq(child.text.inl, ctx)
    textCtx.data ++: child.content.fold(textCtx.empty[Frag])((singleSast: Sast) => convertSast(textCtx, singleSast))

  def categoriesSpan(categories: Seq[String]): Option[Tag] =
    Option.when(categories.nonEmpty)(
      span(cls := "category")(categories.map(c => stringFrag(s" $c "))*)
    )

  def tMeta(article: TitledArticle): Frag =

    def timeFull(date: ScitzenDateTime): Tag = time(date.full)

    val categories = List("categories", "people").flatMap(article.named.get)
      .flatMap(_.split(","))

    val metalist =
      article.date.map(timeFull) ++
      categoriesSpan(categories) ++
      article.named.get("folder").map(f => span(cls := "category")(stringFrag(s" in $f")))

    if metalist.nonEmpty then div(cls := "metadata")(metalist.toSeq*) else frag()

  def articleHeader(article: TitledArticle)(ctx: Cta): Ctx[Frag] =
    convertInlineSeq(article.header.titleText.inl, ctx).map { innerFrags =>
      frag(h1(id := article.header.ref, innerFrags.convert), tMeta(article))
    }

  override def convertSection(section: Section, ctx: Cta): CtxCF =
    val Section(title, level, _) = section
    convertInlineSeq(title.inl, ctx).map { innerFrags =>
      val addDepth: Int =
        if level.contains("=") then 0
        else
          ctx.sections.iterator
            .map(_.prefix)
            .find(_.contains("="))
            .fold(1)(s => s.length)
      Chain[Frag](tag(s"h${level.length + addDepth}")(id := section.ref, innerFrags.convert))
    }.push(section)

  override def convertSlist(slist: Slist, ctx: Cta): CtxCF = slist match
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
            val inlinesCtx = convertInlineSeq(c.text.inl, ctx)
            c.content.fold(inlinesCtx.empty[Frag])((singleSast: Sast) => convertSast(inlinesCtx, singleSast)).map { innerFrags =>
              Chain(dt(inlinesCtx.data.convert), dd(innerFrags.convert))
            }
          }.map(i => Chain(dl(i.convert)))

  override def convertBlockDirective(directive: Directive, ctx: Cta): CtxCF =
    val attributes = directive.attributes
    directive.command match
      case Other("aggregate") => handleAggregate(ctx, directive)
      case Other("break") =>
        ctx.ret(Chain(hr))

      case Other("article") =>
        def timeShort(date: Option[String]) = time(f"${date.getOrElse("")}%-8s")

        val aref = attributes.named("target")

        ctx.ret(Chain(tags2.article(
          timeShort(attributes.named.get("datetime")),
          " ", // whitespace prevents elements from hugging without stylesheet
          a(
            cls  := "title",
            href := aref,
            attributes.target
          ),
          " ",
          categoriesSpan(attributes.raw.collect { case Plain("category", value) => value })
        )))

      case Include => handleInclude(ctx, directive)

      case other =>
        convertInlineSeq(List(directive), ctx)
  end convertBlockDirective

  def convertBlock(block: Block, ctx: Cta): CtxCF =

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

    block.attributes.nestedMap.get("note").fold(innerCtx) { note =>
      convertInlineSeq(note.targetT.inl, innerCtx).map { content =>
        innerCtx.data :+ p(`class` := "marginnote", content.convert)
      }
    }
  end convertBlock

  def convertStandardBlock(block: Block, ctx: Cta): CtxCF = block.content match
    case Paragraph(text) => convertInlineSeq(text.inl, ctx).map(cf => Chain(p(cf.convert)))

    case Parsed(delimiter, blockContent) =>
      convertSastSeq(ctx, blockContent).map { blockContent =>
        if delimiter.isBlank then
          blockContent
        else
          val tag = if block.command == BCommand.Figure then figure
          else section
          val fig = tag(blockContent.convert)
          Chain(block.attributes.named.get("label").fold(fig: Tag)(l => fig(id := l)))
      }

    case Fenced(text) => handleCodeListing(ctx, block, text)

    case SpaceComment(_) => ctx.empty
  end convertStandardBlock

  def handleCodeListing(ctx: Cta, block: Block, text: String): CtxCF =
    // Code listing
    // Use this for monospace, space preserving, line preserving text
    // It may wrap to fit the screen content
    val labeltext =
      if !block.attributes.named.contains("label") then text
      else
        text.replaceAll(""":§([^§]*?)§""", "")
    val initTag: Tag =
      if !block.attributes.legacyPositional.contains("highlight") then
        block.attributes.named.get("lang") match
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
      block.attributes.named.get("lang").fold(pre(initTag))(l => pre(initTag(cls := s"language-${l}")))
    val res = block.attributes.named.get("label").fold(respre: Tag)(l => respre(id := l))
    ctx.useFeature("prism").retc(res)
  end handleCodeListing

  override def convertText(inlineText: InlineText, ctx: Cta): CtxInl = ctx.retc(stringFrag(inlineText.str))
  override def convertDirective(directive: Directive, ctx: Cta): CtxInl =
    val attrs = directive.attributes
    directive.command match
      case Strong => convertInlineSeq(attrs.text.inl, ctx).map(c => strong(c.convert)).single
      case Emph   => convertInlineSeq(attrs.text.inl, ctx).map(c => em(c.convert)).single
      case Code   => ctx.retc(code(attrs.target))
      case Script =>
        doc.resolve(attrs.target) match
          case Some(path) =>
            val rel = project.htmlPaths.relativizeImage(path)
            ctx.retc(script(`type` := "text/javascript", src := rel.toString))
              .requireInOutput(path, rel)
          case None =>
            ctx.retc(warn("no script", directive))

      case Def | Comment => ctx.empty
      case Include       => ctx.retc(unknownMacroOutput(directive))

      case Raw => ctx.retc(div(raw(attrs.named.getOrElse("html", ""))))

      case Math =>
        val inner = attrs.target
        ctx.katex(inner).map(res => Chain(math(raw(res))))

      case BibQuery =>
        convertDirective(anal.bib.convert(directive), ctx)

      case Cite =>
        val citations = anal.bib.bibkeys(directive).map(k => k -> anal.bib.entries.get(k))
        val anchors = citations.sortBy(_._2.map(_.citekey)).flatMap {
          case (bibid, Some(bib)) => List(a(href := s"#$bibid", bib.citekey), stringFrag(",\u2009"))
          case (bibid, None) =>
            scribe.error(s"bib key not found: »${bibid}«" + reportPos(directive))
            List(code(bibid), stringFrag(" "))
        }.dropRight(1)
        val cctx          = ctx.cite(citations.flatMap(_._2))
        val styledAnchors = span(cls := "citations", "(", anchors, ")")
        if attrs.arguments.nonEmpty then
          convertInlineSeq(attrs.argumentsT.head.inl, cctx).map { res =>
            if res.isEmpty then res
            else
              val last = res.last
              val init = res.init
              val addSpace = last match
                case StringFrag(s) => stringFrag(s"$s\u2009")
                case other         => last
              init ++ Chain(addSpace, styledAnchors)
          }
        else if attrs.named.get("style").contains("author")
        then
          val nameOption =
            for
              first  <- citations.headOption
              bi     <- first._2
              author <- bi.authors.headOption
              family <- author.familyName
            yield stringFrag(s"${family}${if bi.authors.sizeIs > 1 then " et al.\u2009" else "\u2009"}")
          cctx.ret(nameOption ++: Chain(styledAnchors))
        else cctx.retc(styledAnchors)

      case Link =>
        val target     = attrs.target
        val contentCtx = convertInlineSeq(attrs.text.inl, ctx)
        contentCtx.mapc(content => a(href := target).apply(content.convert))

      case Ref =>
        val scope =
          attrs.named.get("scope").flatMap(doc.resolve).getOrElse(doc.path)
        val candidates = References.filterCandidates(scope, anal.directory.labels.getOrElse(attrs.target, Nil))

        if candidates.sizeIs > 1 then
          scribe.error(
            s"multiple resolutions for ${attrs.target}" +
            reporter(directive.prov) +
            s"\n\tresolutions are in: ${candidates.map(c => c.scope).mkString("\n\t", "\n\t", "\n\t")}"
          )

        candidates.headOption.map[CtxCF] { (targetDocument: SastRef) =>
          val nameOpt    = attrs.arguments.headOption
          val articleOpt = targetDocument.directArticle
          val fileRef =
            articleOpt match
              case Some(article) =>
                project.htmlPaths.relativeArticleTarget(article).toString
              case _ => ""

          targetDocument.sast match
            case sec @ Section(title, _, _) => convertInlineSeq(title.inl, ctx).map { inner =>
                Chain(a(href := s"$fileRef#${sec.ref}", nameOpt.fold(inner.convert)(n => List(stringFrag(n)))))
              }
            case Block(_, attr, _) =>
              val label = attr.named("label")
              val name  = nameOpt.fold(label)(n => s"$n $label")
              ctx.retc(a(href := s"$fileRef#$label", name))

            case other =>
              scribe.error(s"can not refer to $other")
              ctx.empty
        }.getOrElse {
          scribe.error(s"no resolutions for »${attrs.target}«${reporter(directive)}")
          ctx.retc(code(SastToScimConverter(anal.bib).macroToScim(directive)))
        }

      case Lookup =>
        handleLookup(directive) match
          case Some(res) => convertInlineSeq(res.inl, ctx)
          case None =>
            ctx.retc(code(attrs.target))

      case Other(otherCommand) =>
        otherCommand match
          case "footnote" =>
            val target =
              SastToTextConverter(
                doc,
                anal,
                project.config.rawAttributes,
              ).convertInlinesAsBlock(attrs.targetT.inl, ctx)
            target.mapc: target =>
              a(title := target, "※")

          case tagname @ ("ins" | "del") =>
            ctx.retc(tag(tagname)(attrs.legacyPositional.mkString(", ")))

          case "todo" => ctx.retc(code(`class` := "todo", SastToScimConverter(anal.bib).macroToScim(directive)))
          case "tableofcontents" => ctx.empty
          case "partition"       => ctx.empty
          case "rule"            => ctx.retc(span(attrs.target, `class` := "rule"))

          case _ => ctx.retc(unknownMacroOutput(directive))

      case Image => convertImage(ctx, directive)
  end convertDirective

  private def convertImage(ctx: Cta, mcro: Directive): Ctx[Chain[Tag]] = {
    val attrs  = mcro.attributes
    val target = anal.image.lookup(doc.resolve(attrs.target).get, ImageTarget.Html)
    if Files.exists(target.absolute)
    then
      val path = project.htmlPaths.relativizeImage(target)
      ctx.requireInOutput(target, path).retc {
        val filename  = path.getFileName.toString
        val sizeclass = mcro.attributes.named.get("size").map(s => cls := s"sizing-$s")
        if videoEndings.exists(filename.endsWith) then
          video(src  := path.toString, attr("loop").empty, attr("autoplay").empty, sizeclass)
        else img(src := path.toString, sizeclass, attrs.named.get("css_style").map(style := _))
      }
    else
      scribe.warn(s"could not find path ${target}" + reporter(mcro))
      ctx.empty
  }

  def reportPos(m: Directive): String = reporter(m)

  def unknownMacroOutput(im: Directive): Tag =
    val str = SastToScimConverter(anal.bib).macroToScim(im)
    scribe.warn(s"unknown macro “$str”" + reportPos(im))
    code(str)

end SastToHtmlConverter

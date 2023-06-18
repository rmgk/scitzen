package scitzen.outputs

import de.rmgk.Chain
import scalatags.Text.StringFrag
import scitzen.bibliography.BibEntry
import scitzen.cli.ConversionAnalysis
import scitzen.compat.Logging.scribe
import scitzen.contexts.ConversionContext
import scitzen.extern.{ImageTarget, Prism}
import scitzen.generic.{Article, References, SastRef, TitledArticle}
import scitzen.sast.*
import scitzen.sast.Attribute.Plain
import scitzen.sast.DCommand.*

import scalatags.Text.all.*
import scalatags.Text.tags2
import scalatags.Text.tags2.{math, section, time}

import java.nio.file.Files

class SastToHtmlConverter(
    sourceArticle: Article,
    anal: ConversionAnalysis
):

  val project  = sourceArticle.doc.path.project
  val reporter = sourceArticle.doc.reporter

  type CtxCF  = ConversionContext[Chain[Frag]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[?]

  val syncPos: Int =
//    if sync.exists(_.path == pathManager.cwf) then sync.get._2
//    else
    Int.MaxValue

  def listItemToHtml(child: ListItem)(ctx: Cta): CtxCF =
    val textCtx = inlineValuesToHTML(child.text.inl)(ctx)
    textCtx.data ++: child.content.fold(textCtx.empty[Frag])(convertSingle(_)(textCtx))

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
    inlineValuesToHTML(article.header.titleText.inl)(ctx).map { innerFrags =>
      frag(h1(id := article.header.ref, innerFrags.toList), tMeta(article))
    }

  val videoEndings = List(".mp4", ".mkv", ".webm")

  def convertSeq(b: Seq[Sast])(ctx: Cta): CtxCF = ctx.fold(b)((ctx, sast) => convertSingle(sast)(ctx))
  def convertSingle(singleSast: Sast)(ctx: Cta): CtxCF =
    singleSast match
      case sec @ Section(title, level, _) =>
        inlineValuesToHTML(title.inl)(ctx).map { innerFrags =>
          val addDepth: Int =
            if level.contains("=") then 0
            else
              ctx.sections.iterator
                .map(_.prefix)
                .find(_.contains("="))
                .fold(1)(s => s.length)
          Chain[Frag](tag(s"h${level.length + addDepth}")(id := sec.ref, innerFrags.toList))
        }.push(sec)

      case Slist(Nil) => ctx.empty
      case Slist(children) => children.head.content match
          case None | Some(Slist(_)) =>
            val listTag = if children.head.marker.contains(".") then ol else ul
            ctx.fold[ListItem, Frag](children) { (ctx, c) =>
              listItemToHtml(c)(ctx).map(i => Chain(li(i.toList)))
            }.map(i => Chain(listTag(i.toList)))
          case _ =>
            ctx.fold[ListItem, Frag](children) { (ctx, c) =>
              val inlinesCtx = inlineValuesToHTML(c.text.inl)(ctx)
              c.content.fold(inlinesCtx.empty[Frag])(convertSingle(_)(inlinesCtx)).map { innerFrags =>
                Chain(dt(inlinesCtx.data.toList*), dd(innerFrags.toList))
              }
            }.map(i => Chain(dl(i.toList)))

      case mcro: Directive =>
        val attributes = mcro.attributes
        mcro.command match
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

          case Include =>
            attributes.arguments.headOption match
              case Some("code") =>
                sourceArticle.doc.resolve(attributes.target) match
                  case None => inlineValuesToHTML(List(mcro))(ctx)
                  case Some(file) =>
                    convertSingle(Block(BCommand.Code, attributes, Fenced(Files.readString(file.absolute)))(mcro.prov))(
                      ctx
                    )

              case None =>
                if attributes.target.endsWith(".scim") then
                  scribe.error(s"including by path no longer supported" + sourceArticle.doc.reporter(mcro))
                  ctx.empty
                else
                  anal.directory.itemsAndArticlesByLabel.get(attributes.target) match
                    case None =>
                      scribe.error(
                        s"unknown include article ${attributes.target}" + sourceArticle.doc.reporter(mcro.prov)
                      )
                      ctx.empty
                    case Some(article) =>
                      new SastToHtmlConverter(
                        article.article,
                        anal
                      ).convertSeq(article.article.sast)(ctx)

              case Some(other) =>
                scribe.error(s"unknown include type $other" + sourceArticle.doc.reporter(mcro.prov))
                ctx.empty

          case other =>
            inlineValuesToHTML(List(mcro))(ctx)

      case tLBlock: Block =>
        if tLBlock.command == BCommand.Convert
        then
          val substitute = anal.block.mapping(tLBlock)
          convertSeq(substitute)(ctx)
        else
          val positiontype = tLBlock.attributes.legacyPositional.headOption
          (positiontype, tLBlock.content) match
            case (Some("quote"), Parsed(_, content)) =>
              convertSeq(content)(ctx).mapc { innerHtml =>
                blockquote(innerHtml.toList)
              }
            case _ =>
              val prov = tLBlock.prov
              convertBlock(tLBlock)(ctx).map { html =>
                if prov.start <= syncPos && syncPos <= prov.end then
                  scribe.info(s"highlighting $syncPos: $prov")
                  div(id := "highlight") +: html
                else html
              }
  end convertSingle

  def convertBlock(sBlock: Block)(ctx: Cta): CtxCF =
    val innerCtx: CtxCF =
      sBlock.content match

        case Paragraph(text) => inlineValuesToHTML(text.inl)(ctx).map(cf => Chain(p(cf.toList)))

        case Parsed(delimiter, blockContent) =>
          convertSeq(blockContent)(ctx).map { blockContent =>
            if delimiter.isBlank then blockContent
            else
              val tag = if sBlock.command == BCommand.Figure then figure else section
              val fig = tag(blockContent.toList)
              Chain(sBlock.attributes.named.get("label").fold(fig: Tag)(l => fig(id := l)))
          }

        case Fenced(text) => sBlock.attributes.named.get(ImageTarget.Html.name) match
            case Some(target) =>
              convertSingle(Directive(
                Image,
                sBlock.attributes.remove(ImageTarget.Html.name).append(List(Attribute("", target)))
              )(
                sBlock.prov
              ))(ctx)
            case None =>
              sBlock.command match
                // convert scala to js and embed the result
//                case BCommand.Embed if sBlock.attributes.named.get("lang").contains("scala") =>
//                  val source = if sBlock.attributes.named.contains("template") then
//                    ImageConverter.applyTemplate(
//                      sBlock.attributes,
//                      text,
//                      sourceArticle.sourceDoc.path.directory,
//                      sourceArticle.sourceDoc.path.project,
//                      preprocessed
//                    )
//                  else text
//                  val js = ScalaCLI.compile(project.cacheDir, source)
//                  ctx.retc(script(`type` := "text/javascript", js.map(raw(_))))

                // Code listing
                // Use this for monospace, space preserving, line preserving text
                // It may wrap to fit the screen content
                case _ =>
                  val labeltext =
                    if !sBlock.attributes.named.contains("label") then text
                    else
                      text.replaceAll(""":§([^§]*?)§""", "")
                  val initTag: Tag =
                    if !sBlock.attributes.legacyPositional.contains("highlight") then
                      sBlock.attributes.named.get("lang") match
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
                    sBlock.attributes.named.get("lang").fold(pre(initTag))(l => pre(initTag(cls := s"language-${l}")))
                  val res = sBlock.attributes.named.get("label").fold(respre: Tag)(l => respre(id := l))
                  ctx.useFeature("prism").retc(res)

        case SpaceComment(_) => ctx.empty

    sBlock.attributes.nestedMap.get("note").fold(innerCtx) { note =>
      inlineValuesToHTML(note.targetT.inl)(innerCtx).map { content =>
        innerCtx.data :+ p(`class` := "marginnote", content.toList)
      }
    }
  end convertBlock

  def inlineValuesToHTML(inlines: Seq[Inline])(ctx: Cta): CtxCF =
    ctx.fold(inlines) { (ctx, inline) => inlineToHTML(inline)(ctx) }

  def inlineToHTML(inlineSast: Inline)(ctx: Cta): CtxCF =
    inlineSast match
      case InlineText(str) => ctx.retc(stringFrag(str))

      case mcro: Directive =>
        val attrs = mcro.attributes
        mcro.command match
          case Strong => inlineValuesToHTML(attrs.text.inl)(ctx).map(c => strong(c.toList)).single
          case Emph   => inlineValuesToHTML(attrs.text.inl)(ctx).map(c => em(c.toList)).single
          case Code   => ctx.retc(code(attrs.target))

          case Def | Comment => ctx.empty
          case Include       => ctx.retc(unknownMacroOutput(mcro))

          case Raw => ctx.retc(div(raw(attrs.named.getOrElse("html", ""))))

          case Math =>
            val inner = attrs.target
            ctx.katex(inner).map(res => Chain(math(raw(res))))

          case BibQuery =>
            inlineToHTML(anal.bib.convert(mcro))(ctx)

          case Cite =>
            val citations = anal.bib.bibkeys(mcro).map(k => k -> anal.bib.entries.get(k))
            val anchors = citations.sortBy(_._2.map(_.citekey)).flatMap {
              case (bibid, Some(bib)) => List(a(href := s"#$bibid", bib.citekey), stringFrag(",\u2009"))
              case (bibid, None) =>
                scribe.error(s"bib key not found: »${bibid}«" + reportPos(mcro))
                List(code(bibid), stringFrag(" "))
            }.dropRight(1)
            val cctx          = ctx.cite(citations.flatMap(_._2))
            val styledAnchors = span(cls := "citations", "(", anchors, ")")
            if attrs.arguments.nonEmpty then
              inlineValuesToHTML(attrs.argumentsT.head.inl)(cctx).map { res =>
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
            val contentCtx = inlineValuesToHTML(attrs.text.inl)(ctx)
            contentCtx.mapc(content => a(href := target)(content.toList))

          case Ref =>
            val scope =
              attrs.named.get("scope").flatMap(sourceArticle.doc.resolve).getOrElse(sourceArticle.doc.path)
            val candidates = References.filterCandidates(scope, anal.directory.labels.getOrElse(attrs.target, Nil))

            if candidates.sizeIs > 1 then
              scribe.error(
                s"multiple resolutions for ${attrs.target}" +
                reporter(mcro.prov) +
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
                case sec @ Section(title, _, _) => inlineValuesToHTML(title.inl)(ctx).map { inner =>
                    Chain(a(href := s"$fileRef#${sec.ref}", nameOpt.fold(inner.toList)(n => List(stringFrag(n)))))
                  }
                case Block(_, attr, _) =>
                  val label = attr.named("label")
                  val name  = nameOpt.fold(label)(n => s"$n $label")
                  ctx.retc(a(href := s"$fileRef#$label", name))

                case other =>
                  scribe.error(s"can not refer to $other")
                  ctx.empty
            }.getOrElse {
              scribe.error(s"no resolutions for »${attrs.target}«${reporter(mcro)}")
              ctx.retc(code(SastToScimConverter(anal.bib).macroToScim(mcro)))
            }

          case Lookup =>
            if project.definitions.contains(attrs.target) then
              inlineValuesToHTML(project.definitions(attrs.target).inl)(ctx)
            else
              scribe.warn(s"unknown name ${attrs.target}" + reporter(mcro))
              ctx.retc(code(attrs.target))

          case Other(otherCommand) =>
            otherCommand match
              case "footnote" =>
                val target =
                  SastToTextConverter(
                    project.config.definitions,
                    anal.directory
                  ).convertInline(attrs.targetT.inl)
                ctx.retc(a(title := target, "※"))

              case tagname @ ("ins" | "del") =>
                ctx.retc(tag(tagname)(attrs.legacyPositional.mkString(", ")))

              case "todo" => ctx.retc(code(`class` := "todo", SastToScimConverter(anal.bib).macroToScim(mcro)))
              case "tableofcontents" => ctx.empty
              case "partition"       => ctx.empty
              case "rule"            => ctx.retc(span(attrs.target, `class` := "rule"))

              case _ => ctx.retc(unknownMacroOutput(mcro))

          case Image => convertImage(ctx, mcro)
  end inlineToHTML

  private def convertImage(ctx: Cta, mcro: Directive): Ctx[Chain[Tag]] = {
    val attrs  = mcro.attributes
    val target = anal.image.lookup(sourceArticle.doc.resolve(attrs.target).get, ImageTarget.Html)
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

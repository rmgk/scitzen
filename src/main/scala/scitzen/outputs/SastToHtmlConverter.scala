package scitzen.outputs

import better.files._
import cats.data.Chain
import cats.implicits._
import scalatags.generic
import scalatags.generic.Bundle
import scitzen.contexts.ConversionContext
import scitzen.extern.Bibliography.BibEntry
import scitzen.extern.ImageTarget
import scitzen.generic.{Article, DocumentDirectory, HtmlPathManager, PreprocessedResults, References, Reporter, SastRef}
import scitzen.sast.MacroCommand._
import scitzen.sast._

class SastToHtmlConverter[Builder, Output <: FragT, FragT](
    val bundle: Bundle[Builder, Output, FragT],
    pathManager: HtmlPathManager,
    bibliography: Map[String, BibEntry],
    sync: Option[(File, Int)],
    reporter: Reporter,
    preprocessed: PreprocessedResults,
):

  import bundle.all._
  import bundle.tags2.{article, section, time}

  type CtxCF  = ConversionContext[Chain[Frag]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[?]

  def includeResolver: DocumentDirectory = preprocessed.directory
  def articles: List[Article]            = preprocessed.articles

  val syncPos: Int =
    if sync.exists(_._1 == pathManager.cwf) then sync.get._2
    else Int.MaxValue

  def listItemToHtml(child: ListItem)(ctx: Cta): CtxCF =
    val textCtx = inlineValuesToHTML(child.text.inl)(ctx)
    textCtx.data ++: child.content.fold(textCtx.empty[Frag])(convertSingle(_)(textCtx))

  def categoriesSpan(categories: Seq[String]): Option[Tag] =
    Option.when(categories.nonEmpty)(
      span(cls := "category")(categories.map(c => stringFrag(s" $c "))*)
    )

  def tMeta(article: Article): generic.Frag[Builder, FragT] =

    def timeFull(date: ScitzenDateTime): Tag = time(date.full)

    val categories = List("categories", "people").flatMap(article.named.get)
      .flatMap(_.split(","))

    val metalist =
      article.date.map(timeFull) ++
        categoriesSpan(categories) ++
        article.named.get("folder").map(f => span(cls := "category")(stringFrag(s" in $f")))

    if metalist.nonEmpty then div(cls := "metadata")(metalist.toSeq*) else frag()

  def articleHeader(article: Article)(ctx: Cta): Ctx[Frag] =
    inlineValuesToHTML(article.header.title.inl)(ctx).map { innerFrags =>
      frag(h1(id := article.header.ref, innerFrags.toList), tMeta(article))
    }

  val videoEndings = List(".mp4", ".mkv", ".webm")

  def convertSeq(b: Seq[Sast])(ctx: Cta): CtxCF = ctx.fold(b)((ctx, sast) => convertSingle(sast)(ctx))
  def convertSingle(singleSast: Sast)(ctx: Cta): CtxCF =
    singleSast match
      case sec @ Section(title, level, _, _) =>
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

      case mcro: Macro =>
        val attributes = mcro.attributes
        mcro.command match
          case Other("break") =>
            ctx.ret(Chain(hr))

          case Other("article") =>
            def timeShort(date: Option[String]) = time(f"${date.getOrElse("")}%-8s")

            val aref = attributes.named("target")

            ctx.ret(Chain(a(
              href := aref,
              article(
                timeShort(attributes.named.get("datetime")),
                span(cls := "title", attributes.target),
                categoriesSpan(attributes.raw.filter(_.id == "category").map(_.value))
              )
            )))

          case Include =>
            attributes.arguments.headOption match
              case Some("code") =>
                pathManager.resolve(attributes.target) match
                  case None => inlineValuesToHTML(List(mcro))(ctx)
                  case Some(file) =>
                    convertSingle(Block(attributes, Fenced(file.contentAsString), mcro.prov))(ctx)

              case None =>
                pathManager.resolve(attributes.target) match
                  case Some(file) =>
                    val doc = includeResolver.byPath(file)
                    new SastToHtmlConverter(
                      bundle,
                      pathManager.changeWorkingFile(file),
                      bibliography,
                      sync,
                      doc.reporter,
                      preprocessed,
                    ).convertSeq(doc.sast)(ctx)
                  case None =>
                    scribe.error(s"unknown include ${attributes.target}" + reporter(mcro.prov))
                    ctx.empty

              case Some(other) =>
                scribe.error(s"unknown include type $other" + reporter(mcro.prov))
                ctx.empty

          case other =>
            inlineValuesToHTML(List(mcro))(ctx)

      case tLBlock: Block =>
        val positiontype = tLBlock.attributes.positional.headOption
        positiontype match
          case Some("quote") =>
            convertBlock(tLBlock)(ctx).map { innerHtml =>
              // for blockquote layout, see example 12 (the twitter quote)
              // http://w3c.github.io/html/textlevel-semantics.html#the-cite-element
              val bq = blockquote(innerHtml.toList)
              // first argument is "quote" we concat the rest and treat them as a single entity
              val title = tLBlock.attributes.positional.drop(1)
              Chain(if title.nonEmpty then bq(cite(title)) else bq)
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
              val tag = if sBlock.command == "figure" then figure else section
              val fig = tag(blockContent.toList)
              Chain(sBlock.attributes.named.get("label").fold(fig: Tag)(l => fig(id := l)))
          }

        case Fenced(text) =>
          if sBlock.attributes.named.contains(ImageTarget.Html.name) then
            val target = sBlock.attributes.named(ImageTarget.Html.name)
            convertSingle(Macro(
              Image,
              sBlock.attributes.remove(ImageTarget.Html.name).append(List(Attribute("", target))),
              sBlock.prov
            ))(ctx)
          else
            sBlock.command match
              // Preformatted plaintext, preserve linebreaks,
              // but also wrap for linebreaks
              case "text" => ctx.retc(pre(text))
              // Code listing
              // Use this for monospace, space preserving, line preserving text
              // It may wrap to fit the screen content
              case _ =>
                val labeltext =
                  if !sBlock.attributes.named.contains("label") then text
                  else
                    text.replaceAll(""":§([^§]*?)§""", "")
                val initTag: Tag =
                  if !sBlock.attributes.positional.contains("highlight") then pre(code(labeltext))
                  else
                    val lines = labeltext.linesIterator.zipWithIndex.filter { case (s, _) => s.contains(":hl§") }.map {
                      _._2 + 1
                    }.mkString(",")
                    val txt = labeltext.replaceAll(""":hl§([^§]*?)§""", "$1")
                    pre(code(txt, attr("data-line-numbers") := lines))

                val respre = sBlock.attributes.named.get("lang").fold(initTag)(l => initTag(cls := l))
                val res    = sBlock.attributes.named.get("label").fold(respre: Tag)(l => respre(id := l))
                ctx.retc(res)

        case SpaceComment(_) => ctx.empty

    sBlock.attributes.namedT.get("note").fold(innerCtx) { note =>
      inlineValuesToHTML(note.inl)(innerCtx).map { content =>
        innerCtx.data :+ p(`class` := "marginnote", content.toList)
      }
    }
  end convertBlock

  def inlineValuesToHTML(inlines: Seq[Inline])(ctx: Cta): CtxCF =
    ctx.fold(inlines) { (ctx, inline) => inlineToHTML(inline)(ctx) }

  def inlineToHTML(inlineSast: Inline)(ctx: Cta): CtxCF =
    inlineSast match
      case InlineText(str) => ctx.retc(stringFrag(str))

      case mcro: Macro =>
        val attrs = mcro.attributes
        mcro.command match
          case Strong => ctx.retc(strong(attrs.target))
          case Emph   => ctx.retc(em(attrs.target))
          case Code   => ctx.retc(code(attrs.target))

          case Def | Comment => ctx.empty
          case Include       => ctx.retc(unknownMacroOutput(mcro))

          case Other("raw") => ctx.retc(div(raw(attrs.named.getOrElse("html", ""))))

          case Math =>
            val inner = attrs.target
            ctx.katex(inner).map(res => Chain(span(raw(res))))

          case Cite =>
            val citations = attrs.target.split(",").toList.map { bibid =>
              bibid -> bibliography.get(bibid.trim)
            }
            val anchors = citations.sortBy(_._2.map(_.citekey)).map {
              case (bibid, Some(bib)) => a(href := s"#$bibid", bib.citekey)
              case (bibid, None) =>
                scribe.error(s"bib key not found: »${bibid.trim}«" + reportPos(mcro))
                code(bibid.trim)
            }
            val cctx = ctx.cite(citations.flatMap(_._2))
            if attrs.arguments.nonEmpty then
              cctx.retc(frag(s"${attrs.arguments.head}\u00A0", anchors))
            else cctx.retc(anchors)

          case Link =>
            val target = attrs.target
            val content = attrs.positionalT.headOption
              .fold(ctx.retc(stringFrag(target))) { txt =>
                inlineValuesToHTML(txt.inl)(ctx)
              }
            content.retc(a(href := target)(content.data.toList))

          case Ref =>
            val scope      = attrs.named.get("scope").flatMap(pathManager.resolve).getOrElse(pathManager.cwf)
            val candidates = References.filterCandidates(scope, preprocessed.labels.getOrElse(attrs.target, Nil))

            if candidates.sizeIs > 1 then
              scribe.error(
                s"multiple resolutions for ${attrs.target}" +
                  reporter(mcro.prov) +
                  s"\n\tresolutions are in: ${candidates.map(c => pathManager.relativizeToProject(c.scope)).mkString("\n\t", "\n\t", "\n\t")}"
              )

            candidates.headOption.map[CtxCF] { (targetDocument: SastRef) =>
              val nameOpt    = attrs.arguments.headOption
              val articleOpt = targetDocument.directArticle
              val fileRef =
                articleOpt match
                  case Some(article) =>
                    pathManager.relativeArticleTarget(article).toString
                  case _ => ""

              targetDocument.sast match
                case sec @ Section(title, _, _, _) => inlineValuesToHTML(title.inl)(ctx).map { inner =>
                    Chain(a(href := s"$fileRef#${sec.ref}", nameOpt.fold(inner.toList)(n => List(stringFrag(n)))))
                  }
                case Block(attr, _, _) =>
                  val label = attr.named("label")
                  val name  = nameOpt.fold(label)(n => s"$n $label")
                  ctx.retc(a(href := s"$fileRef#$label", name))

                case other =>
                  scribe.error(s"can not refer to $other")
                  ctx.empty
            }.getOrElse {
              scribe.error(s"no resolutions for »${attrs.target}«${reporter(mcro)}")
              ctx.retc(code(SastToScimConverter.macroToScim(mcro)))
            }

          case Lookup =>
            if pathManager.project.definitions.contains(attrs.target) then
              inlineValuesToHTML(pathManager.project.definitions(attrs.target).inl)(ctx)
            else
              scribe.warn(s"unknown name ${attrs.target}" + reporter(mcro))
              ctx.retc(code(attrs.target))

          case Other(otherCommand) =>
            otherCommand match
              case "footnote" =>
                val target =
                  SastToTextConverter(
                    pathManager.project.config.definitions,
                    Some(Includes(pathManager.project, pathManager.cwf, includeResolver))
                  ).convertInline(attrs.targetT.inl)
                ctx.retc(a(title := target, "※"))

              case tagname @ ("ins" | "del") =>
                ctx.retc(tag(tagname)(attrs.positional.mkString(", ")))

              case "todo"            => ctx.retc(code(`class` := "todo", SastToScimConverter.macroToScim(mcro)))
              case "tableofcontents" => ctx.empty
              case "partition"       => ctx.empty
              case "rule"            => ctx.retc(span(attrs.target, `class` := "rule"))

              case _ => ctx.retc(unknownMacroOutput(mcro))

          case Image =>
            val target = attrs.named.getOrElse(ImageTarget.Html.name, attrs.target)
            pathManager.project.resolve(pathManager.cwd, target) match
              case Some(target) =>
                val path = pathManager.relativizeImage(target)
                val mw   = java.lang.Double.parseDouble(attrs.named.getOrElse("maxwidth", "1")) * 100
                ctx.requireInOutput(target, path).retc {
                  val filename = path.getFileName.toString
                  if videoEndings.exists(filename.endsWith) then
                    video(src := path.toString, attr("loop").empty, attr("autoplay").empty)
                  else img(src := path.toString, style := s"max-width: $mw%")
                }
              case None =>
                scribe.warn(s"could not find path ${target}" + reporter(mcro))
                ctx.empty
  end inlineToHTML

  def reportPos(m: Macro): String = reporter(m)

  def unknownMacroOutput(im: Macro): Tag =
    val str = SastToScimConverter.macroToScim(im)
    scribe.warn(s"unknown macro “$str”" + reportPos(im))
    code(str)

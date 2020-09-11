package scitzen.outputs

import better.files._
import cats.data.Chain
import cats.implicits._
import scalatags.generic
import scalatags.generic.Bundle
import scitzen.extern.Bibliography.BibEntry
import scitzen.generic.{Article, ConversionContext, DocumentDirectory, HtmlPathManager, References, Reporter, SastRef}
import scitzen.sast.MacroCommand._
import scitzen.sast._

class SastToHtmlConverter[Builder, Output <: FragT, FragT](
    val bundle: Bundle[Builder, Output, FragT],
    pathManager: HtmlPathManager,
    bibliography: Map[String, BibEntry],
    sync: Option[(File, Int)],
    reporter: Reporter,
    includeResolver: DocumentDirectory,
    articles: List[Article]
) {

  import bundle.all._
  import bundle.tags2.{article, section, time}

  type CtxCF  = ConversionContext[Chain[Frag]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[_]

  val syncPos: Int =
    if (sync.exists(_._1 == pathManager.cwf)) sync.get._2
    else Int.MaxValue

  def listItemToHtml(child: ListItem)(implicit ctx: Cta): CtxCF = {
    val textCtx = inlineValuesToHTML(child.text.inl)(ctx)
    textCtx.data ++: child.content.fold(textCtx)(convertSingle(_)(textCtx))
  }

  def categoriesSpan(categories: Seq[String]): Option[Tag] = {
    Option.when(categories.nonEmpty)(
      span(cls := "category")(categories.map(c => stringFrag(s" $c ")): _*)
    )
  }

  def tMeta(article: Article): generic.Frag[Builder, FragT] = {

    def timeFull(date: ScitzenDateTime): Tag = time(date.full)

    val categories = List("categories", "people").flatMap(article.named.get)
      .flatMap(_.split(","))

    val metalist =
      article.date.map(timeFull) ++
        categoriesSpan(categories) ++
        article.named.get("folder").map(f => span(cls := "category")(stringFrag(s" in $f")))

    if (metalist.nonEmpty) div(cls := "metadata")(metalist.toSeq: _*) else frag()
  }

  def articleHeader(article: Article)(ctx: Cta): Ctx[Frag] = {
    inlineValuesToHTML(article.header.title.inl)(ctx).map { innerFrags =>
      frag(h1(id := article.header.ref, innerFrags.toList), tMeta(article))
    }
  }

  def convertSeq(b: Seq[Sast])(implicit ctx: Cta): CtxCF = ctx.fold(b)((ctx, sast) => convertSingle(sast)(ctx))
  def convertSingle(singleSast: Sast)(implicit ctx: Cta): CtxCF = {
    singleSast match {
      case sec @ Section(title, level, _) =>
        inlineValuesToHTML(title.inl)(ctx).map { innerFrags =>
          val addDepth: Int =
            if (level.contains("=")) 0
            else
              ctx.sections.iterator
                .map(_.prefix)
                .find(_.contains("="))
                .fold(0)(s => s.length)
          Chain[Frag](tag(s"h${level.length + addDepth}")(id := sec.ref, innerFrags.toList))
        }.push(sec)

      case Slist(Nil) => ctx.empty
      case Slist(children) => children.head.content match {
          case None | Some(Slist(_)) =>
            val listTag = if (children.head.marker.contains(".")) ol else ul
            ctx.fold[ListItem, Frag](children) { (ctx, c) =>
              listItemToHtml(c)(ctx).map(i => Chain(li(i.toList)))
            }.map(i => Chain(listTag(i.toList)))
          case _ =>
            ctx.fold[ListItem, Frag](children) { (ctx, c) =>
              val inlinesCtx = inlineValuesToHTML(c.text.inl)(ctx)
              c.content.fold(inlinesCtx.empty[Frag])(convertSingle(_)(inlinesCtx)).map { innerFrags =>
                Chain(dt(inlinesCtx.data.toList: _*), dd(innerFrags.toList))
              }
            }.map(i => Chain(dl(i.toList)))
        }

      case mcro @ Macro(_, _) => mcro match {
          case Macro(Image, attributes) =>
            pathManager.project.resolve(pathManager.cwd, attributes.target) match {
              case Some(target) =>
                val path = pathManager.relativizeImage(target)
                ctx.requireInOutput(target, path).retc {
                  img(src := path.toString)(attributes.named.get("style").map(style := _))
                }
              case None =>
                scribe.warn(s"could not find path ${attributes.target}" + reporter(mcro))
                ctx.empty
            }

          case Macro(Other("break"), _) =>
            ctx.ret(Chain(hr))

          case Macro(Other("article"), attributes) =>
            def timeShort(date: Option[String]) =
              date match {
                case Some(date) => time(stringFrag(date + " "))
                case None       => time("00-00 00:00")
              }

            val aref = attributes.named("target")

            ctx.ret(Chain(a(
              href := aref,
              article(
                timeShort(attributes.named.get("datetime")),
                span(cls := "title", attributes.target),
                categoriesSpan(attributes.raw.filter(_.id == "category").map(_.value))
              )
            )))

          case Macro(Include, attributes) =>
            attributes.arguments.headOption match {
              case Some("code") =>
                pathManager.resolve(attributes.target) match {
                  case None => inlineValuesToHTML(List(mcro))
                  case Some(file) =>
                    convertSingle(Block(attributes, Fenced(file.contentAsString)))
                }

              case None =>
                pathManager.resolve(attributes.target) match {
                  case Some(file) =>
                    val doc = includeResolver.byPath(file)
                    new SastToHtmlConverter(
                      bundle,
                      pathManager.changeWorkingFile(file),
                      bibliography,
                      sync,
                      doc.reporter,
                      includeResolver,
                      articles
                    ).convertSeq(doc.sast)(ctx)
                  case None =>
                    scribe.error(s"unknown include ${attributes.target}" + reporter(attributes.prov))
                    ctx.empty
                }

              case Some(other) =>
                scribe.error(s"unknown include type $other" + reporter(attributes.prov))
                ctx.empty
            }

          case other =>
            inlineValuesToHTML(List(other))
        }

      case tLBlock: Block =>
        val positiontype = tLBlock.attributes.positional.headOption
        positiontype match {
          case Some("quote") =>
            convertBlock(tLBlock).map { innerHtml =>
              // for blockquote layout, see example 12 (the twitter quote)
              // http://w3c.github.io/html/textlevel-semantics.html#the-cite-element
              val bq = blockquote(innerHtml.toList)
              // first argument is "quote" we concat the rest and treat them as a single entity
              val title = tLBlock.attributes.positional.drop(1)
              Chain(if (title.nonEmpty) bq(cite(title)) else bq)
            }
          case _ =>
            val prov = tLBlock.attributes.prov
            convertBlock(tLBlock).map { html =>
              if (prov.start <= syncPos && syncPos <= prov.end) {
                scribe.info(s"highlighting $syncPos: $prov")
                div(id := "highlight") +: html
              } else html
            }
        }

    }
  }

  def convertBlock(sBlock: Block)(implicit ctx: Cta): CtxCF = {
    val innerCtx: CtxCF = sBlock.content match {

      case Paragraph(text) => inlineValuesToHTML(text.inl).map(cf => Chain(p(cf.toList)))

      case Parsed(delimiter, blockContent) =>
        convertSeq(blockContent).map { blockContent =>
          if (delimiter.isBlank) blockContent
          else {
            val tag = if (sBlock.command == "figure") figure else section
            val fig = tag(blockContent.toList)
            Chain(sBlock.attributes.named.get("label").fold(fig: Tag)(l => fig(id := l)))
          }
        }

      case Fenced(text) => sBlock.attributes.positional.headOption match {
          // Preformatted plaintext, preserve linebreaks,
          // but also wrap for linebreaks
          case Some("text") => ctx.retc(pre(text))
          // Code listing
          // Use this for monospace, space preserving, line preserving text
          // It may wrap to fit the screen content
          case _ =>
            val txt =
              if (!sBlock.attributes.named.contains("label")) text
              else {
                text.replaceAll(""":§([^§]*?)§""", "")
              }
            val respre = pre(code(txt))
            val res    = sBlock.attributes.named.get("label").fold(respre: Tag)(l => respre(id := l))
            ctx.retc(res)
        }

      case SpaceComment(_) => ctx.empty
    }

    sBlock.attributes.namedT.get("note").fold(innerCtx) { note =>
      inlineValuesToHTML(note.inl)(innerCtx).map { content =>
        innerCtx.data :+ p(`class` := "marginnote", content.toList)
      }
    }
  }

  def inlineValuesToHTML(inlines: Seq[Inline])(implicit ctx: Cta): CtxCF =
    ctx.fold(inlines) { (ctx, inline) => inlineToHTML(inline)(ctx) }

  def inlineToHTML(inlineSast: Inline)(implicit ctx: Cta): CtxCF =
    inlineSast match {
      case InlineText(str) => ctx.retc(stringFrag(str))

      case Macro(Strong, attrs)             => ctx.retc(strong(attrs.target))
      case Macro(Emph, attrs)               => ctx.retc(em(attrs.target))
      case Macro(Code, attrs)               => ctx.retc(code(attrs.target))
      case Macro(Other("partition"), attrs) => ctx.empty

      case Macro(Math, attrs) =>
        val inner = attrs.target
        ctx.katex(inner).map(res => Chain(span(raw(res))))

      case Macro(Comment, _) => ctx.empty

      case mcro @ Macro(Cite, attributes) =>
        val citations = attributes.target.split(",").toList.map { bibid =>
          bibid -> bibliography.get(bibid.trim)
        }
        val anchors = citations.sortBy(_._2.map(_.citekey)).map {
          case (bibid, Some(bib)) => a(href := s"#$bibid", bib.citekey)
          case (bibid, None) =>
            scribe.error(s"bib key not found: »${bibid.trim}«" + reportPos(mcro))
            code(bibid.trim)
        }
        val cctx = ctx.cite(citations.flatMap(_._2))
        if (attributes.arguments.nonEmpty) {
          cctx.retc(frag(s"${attributes.arguments.head}\u00A0", anchors))
        } else cctx.retc(anchors)

      case Macro(Label, _) => ctx.empty

      case Macro(Link, attributes) =>
        val target = attributes.target
        ctx.retc(linkTo(attributes, target))

      case macroRef @ Macro(Ref, attributes) =>
        val scope      = attributes.named.get("scope").flatMap(pathManager.resolve).getOrElse(pathManager.cwf)
        val candidates = References.filterCandidates(scope, ctx.resolveRef(attributes.target))

        if (candidates.sizeIs > 1)
          scribe.error(
            s"multiple resolutions for ${attributes.target}" +
              reporter(attributes.prov) +
              s"\n\tresolutions are in: ${candidates.map(c => pathManager.relativizeToProject(c.scope)).mkString("\n\t", "\n\t", "\n\t")}"
          )

        candidates.headOption.map[CtxCF] { targetDocument: SastRef =>
          val nameOpt = attributes.arguments.headOption
          val articleOpt =
            targetDocument.directArticle.orElse(articles.find(a => a.includes.byPath.contains(targetDocument.scope)))
          val fileRef = articleOpt match {
            case Some(article) if !article.includes.byPath.contains(pathManager.cwf) =>
              pathManager.relativeArticleTarget(article).toString
            case _ => ""
          }

          targetDocument.sast match {
            case sec @ Section(title, _, _) => inlineValuesToHTML(title.inl).map { inner =>
                Chain(a(href := s"$fileRef#${sec.ref}", nameOpt.fold(inner.toList)(n => List(stringFrag(n)))))
              }
            case Block(attr, _) =>
              val label = attr.named("label")
              val name  = nameOpt.fold(label)(n => s"$n $label")
              ctx.retc(a(href := s"$fileRef#$label", name))

            case other =>
              scribe.error(s"can not refer to $other")
              ctx.empty
          }
        }.getOrElse {
          scribe.error(s"no resolutions for »${attributes.target}«${reporter(attributes.prov)}")
          ctx.retc(code(SastToScimConverter.macroToScim(macroRef)))
        }

      case Macro(Lookup, attributes) =>
        if (pathManager.project.config.definitions.contains(attributes.target))
          ctx.retc(pathManager.project.config.definitions(attributes.target))
        else {
          scribe.warn(s"unknown name ${attributes.target}" + reporter(attributes.prov))
          ctx.retc(code(attributes.target))
        }

      case mcro @ Macro(Other(otherCommand), attributes) =>
        otherCommand match {
          case "footnote" =>
            val target =
              SastToTextConverter(pathManager.project.config.definitions).convertInline(attributes.targetT.inl)
            ctx.retc(a(title := target, "※"))

          case "textsc" =>
            ctx.retc(span(`class` := "smallcaps", attributes.target))

          case tagname @ ("ins" | "del") =>
            ctx.retc(tag(tagname)(attributes.positional.mkString(", ")))

          case protocol @ ("http" | "https" | "ftp" | "irc" | "mailto") =>
            val linktarget = s"$protocol:${attributes.target}"
            ctx.retc(linkTo(attributes, linktarget))

          case "subparagraph" => ctx.retc(b(`class` := "paragraphtitle", attributes.target))

          case "todo" => ctx.retc(code(`class` := "todo", SastToScimConverter.macroToScim(mcro)))

          case "tableofcontents" => ctx.empty

          case _ => ctx.retc(unknownMacroOutput(mcro))

        }

      case Macro(Def, _) => ctx.empty

      case mcro @ Macro(Image | Include, _) =>
        ctx.retc(unknownMacroOutput(mcro))

    }

  def reportPos(m: Macro): String = reporter(m)

  def unknownMacroOutput(im: Macro): Tag = {
    val str = SastToScimConverter.macroToScim(im)
    scribe.warn(s"unknown macro “$str”" + reportPos(im))
    code(str)
  }

  def linkTo(attributes: Attributes, linktarget: String): Tag = {
    a(href := linktarget)(attributes.positional.headOption.getOrElse[String](linktarget))
  }
}

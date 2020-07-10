package scitzen.outputs

import java.lang.ProcessBuilder.Redirect

import better.files._
import cats.data.Chain
import cats.implicits._
import scalatags.generic.Bundle
import scitzen.generic.Sast._
import scitzen.generic.{AnalyzedDoc, ConversionContext, HtmlPathManager, Reporter, Sast, SastRef}
import scitzen.parser.MacroCommand.{Cite, Code, Comment, Def, Emph, Image, Include, Label, Link, Lookup, Math, Other, Ref, Strong}
import scitzen.parser.{Attributes, Inline, InlineText, Macro, ScitzenDateTime}

import scala.jdk.CollectionConverters._

class SastToHtmlConverter[Builder, Output <: FragT, FragT](
    val bundle: Bundle[Builder, Output, FragT],
    pathManager: HtmlPathManager,
    bibliography: Map[String, String],
    sdoc: AnalyzedDoc,
    sync: Option[(File, Int)],
    reporter: Reporter,
    includeResolver: Map[File, Seq[Sast]]
) {

  import bundle.all._
  import bundle.tags2.{article, section, time}

  type CtxCF  = ConversionContext[Chain[Frag]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[_]

  val syncPos: Int =
    if (sync.exists(_._1 == pathManager.cwf)) sync.get._2
    else Int.MaxValue

  def listItemToHtml(child: SlistItem)(implicit ctx: Cta): CtxCF = {
    val textCtx = inlineValuesToHTML(child.text.inline)(ctx)
    textCtx.data ++: convertSingle(child.content)(textCtx)
  }

  def categoriesSpan(analyzedDoc: AnalyzedDoc): Option[Tag] = {
    val categories = List("categories", "people").flatMap(analyzedDoc.named.get)
      .flatMap(_.split(","))
    Option.when(categories.nonEmpty)(
      span(cls := "category")(categories.map(c => stringFrag(s" $c ")): _*)
    )
  }

  def tMeta() = {

    def timeFull(date: ScitzenDateTime): Tag = time(date.full)

    val metalist =
      sdoc.date.map(timeFull) ++
        categoriesSpan(sdoc) ++
        sdoc.named.get("folder").map(f => span(cls := "category")(stringFrag(s" in $f")))

    if (metalist.nonEmpty) div(cls := "metadata")(metalist.toSeq: _*) else frag()
  }

  def convertSeq(b: Seq[Sast])(implicit ctx: Cta): CtxCF = ctx.fold(b)((ctx, sast) => convertSingle(sast)(ctx))
  def convertSingle(singleSast: Sast)(implicit ctx: Cta): CtxCF = {
    singleSast match {

      case NoContent => ctx.empty

      case sec @ Section(title, level, _) =>
        val inner = (if (ctx.stack.isEmpty) Chain(tMeta()) else Chain.nil)
        inlineValuesToHTML(title.inline)(ctx).map { innerFrags =>
          tag(s"h${level.length + ctx.stacklevel}")(id := sec.ref, innerFrags.toList) +: inner
        }.push(sec)

      case Slist(Nil) => ctx.empty
      case Slist(children) => children.head.content match {
          case NoContent | Slist(_) =>
            val listTag = if (children.head.marker.contains(".")) ol else ul
            ctx.fold[SlistItem, Frag](children) { (ctx, c) =>
              listItemToHtml(c)(ctx).map(i => Chain(li(i.toList)))
            }.map(i => Chain(listTag(i.toList)))
          case _ =>
            ctx.fold[SlistItem, Frag](children) { (ctx, c) =>
              val inlinesCtx = inlineValuesToHTML(c.text.inline)(ctx)
              convertSingle(c.content)(inlinesCtx).map { innerFrags =>
                Chain(dt(inlinesCtx.data.toList: _*), dd(innerFrags.toList))
              }
            }.map(i => Chain(dl(i.toList)))
        }

      case SMacro(mcro) => mcro match {
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

          case Macro(Other("break"), attributes) =>
            ctx.ret(Chain(hr))

          case Macro(Ref, attributes) if attributes.named.get("type").contains("article") =>
            val foundDoc = pathManager.findDoc(attributes.target)
            val post     = foundDoc.get

            def timeShort(date: Option[ScitzenDateTime]) =
              date match {
                case Some(date) => time(stringFrag(date.monthDayTime + " "))
                case None       => time("00-00 00:00")
              }

            val aref = pathManager.relativePostTarget(post.parsed.file).toString

            ctx.ret(Chain(a(
              href := aref,
              article(
                timeShort(post.analyzed.date),
                span(cls := "title", post.analyzed.title),
                categoriesSpan(post.analyzed)
              )
            )))

          case Macro(Include, attributes) =>
            attributes.arguments.headOption match {
              case Some("code") =>
                pathManager.resolve(attributes.target) match {
                  case None => inlineValuesToHTML(List(mcro))
                  case Some(file) =>
                    convertSingle(SBlock(attributes, Fenced(file.contentAsString)))
                }

              case None =>
                pathManager.findDoc(attributes.target) match {
                  case Some(doc) =>
                    val stack = ctx.stack
                    val sast  = includeResolver(doc.parsed.file)
                    new SastToHtmlConverter(
                      bundle,
                      pathManager.changeWorkingFile(doc.parsed.file),
                      bibliography,
                      doc.analyzed,
                      sync,
                      doc.parsed.reporter,
                      includeResolver
                    )
                      .convertSeq(sast)(ctx.push(singleSast))
                      .copy(stack = stack)
                  case None =>
                    scribe.error(s"unknown include ${attributes.target}" + reporter(attributes.prov))
                    ctx.empty
                }

              case Some(other) =>
                scribe.error(s"unknown include type ${other}" + reporter(attributes.prov))
                ctx.empty
            }

          case other =>
            inlineValuesToHTML(List(other))
        }

      case tLBlock: SBlock =>
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

  def convertBlock(sBlock: SBlock)(implicit ctx: Cta): CtxCF = {
    val innerCtx: CtxCF = sBlock.content match {

      case Paragraph(text) => inlineValuesToHTML(text.inline).map(cf => Chain(p(cf.toList)))

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
          case other =>
            val txt =
              if (!sBlock.attributes.named.contains("label")) text
              else {
                text.replaceAll(""":§([^§]*?)§""", "")
              }
            val respre = pre(code(txt))
            val res    = sBlock.attributes.named.get("label").fold(respre: Tag)(l => respre(id := l))
            ctx.retc(res)
        }

      case SpaceComment(content) => ctx.empty
    }

    sBlock.attributes.named.get("note").fold(innerCtx) { note =>
      innerCtx :+ p(`class` := "marginnote", sBlock.attributes.named("note"))
    }
  }

  def inlineValuesToHTML(inlines: Seq[Inline])(implicit ctx: Cta): CtxCF =
    ctx.fold(inlines) { (ctx, inline) => inlineToHTML(inline)(ctx) }

  def filterCandidates(scope: File, candidates: List[SastRef]): List[SastRef] = {
    candidates match {
      case Nil       => candidates
      case List(one) => candidates
      case multiple =>
        val searchScope = scope.path.iterator().asScala.toList
        val sorted = multiple.map { c =>
          c ->
            c.file.path.iterator().asScala.toList.zip(searchScope).takeWhile {
              case (l, r) => l == r
            }.size
        }.sortBy(_._2).reverse

        val best     = sorted.head._2
        val bestOnly = sorted.takeWhile(_._2 == best)
        (if (bestOnly.size == 1) bestOnly else sorted).map(_._1)
    }

  }

  def inlineToHTML(inline: Inline)(implicit ctx: Cta): CtxCF =
    inline match {
      case InlineText(str) => ctx.retc(stringFrag(str))

      case Macro(Strong, attrs) => ctx.retc(strong(attrs.target))
      case Macro(Emph, attrs)   => ctx.retc(em(attrs.target))
      case Macro(Code, attrs)   => ctx.retc(code(attrs.target))

      case Macro(Math, attrs) =>
        val katexdefs = sdoc.named.get("katexTemplate")
          .flatMap(path => pathManager.project.resolve(pathManager.cwd, path))
        val inner = attrs.target
        val (mathml, ictx) = ctx.katex(
          inner, {
            val pb = katexdefs match {
              case None       => new ProcessBuilder("katex")
              case Some(file) => new ProcessBuilder("katex", "--macro-file", file.pathAsString)
            }
            val process = pb.redirectInput(Redirect.PIPE)
              .redirectOutput(Redirect.PIPE)
              .start()
            process.getOutputStream.writeAndClose(inner)
            process.waitFor()
            process.getInputStream.asString()
          }
        )
        ictx.retc(span(raw(mathml)))

      case Macro(Comment, attributes) => ctx.empty

      case mcro @ Macro(Cite, attributes) =>
        val anchors = attributes.target.split(",").toList.map { bibid =>
          bibid -> bibliography.getOrElse(
            bibid.trim, {
              scribe.error(s"bib key not found: »${bibid.trim}«" + reportPos(mcro))
              bibid
            }
          )
        }.sortBy(_._2).map { case (bibid, bib) => a(href := s"#$bibid", bib) }
        if (attributes.arguments.nonEmpty) {
          ctx.retc(frag(s"${attributes.arguments.head}\u00A0", anchors))
        } else ctx.retc(anchors)

      case Macro(Label, _) => ctx.empty

      case Macro(Link, attributes) =>
        val target = attributes.target
        ctx.retc(linkTo(attributes, target))

      case macroRef @ Macro(Ref, attributes) =>
        pathManager.findDoc(attributes.target) match {
          case Some(fd) =>
            val targetpath = pathManager.relativePostTarget(fd.parsed.file).toString
            val name       = attributes.arguments.headOption.getOrElse(fd.parsed.file.nameWithoutExtension)
            ctx.retc(
              a(href := targetpath, name)
            )

          case None =>
            val scope      = attributes.named.get("scope").flatMap(pathManager.resolve).getOrElse(pathManager.cwf)
            val candidates = filterCandidates(scope, ctx.resolveRef(attributes.target))

            if (candidates.sizeIs > 1)
              scribe.error(
                s"multiple resolutions for ${attributes.target}" +
                  reporter(attributes.prov) +
                  s"\n\tresolutinos are in: ${candidates.map(c => pathManager.relativizeToProject(c.file)).mkString("\n\t", "\n\t", "\n\t")}"
              )

            candidates.headOption.map[CtxCF] { target =>
              target.sast match {
                case sec @ Section(title, _, _) => inlineValuesToHTML(title.inline).map { inner =>
                    Chain(a(href := s"#${sec.ref}", inner.toList))
                  }
                case block @ SBlock(attr, content) =>
                  val label = attr.named("label")
                  val name =
                    attributes.arguments.headOption
                      .fold(label)(n => s"$n $label")
                  ctx.retc(a(href := s"#${label}", name))

                case other =>
                  scribe.error(s"can not refer to $other")
                  ctx.empty
              }
            }.getOrElse {
              scribe.error(s"no resolutions for »${attributes.target}«${reporter(attributes.prov)}")
              ctx.retc(unknownMacroOutput(macroRef))
            }
        }

      case Macro(Lookup, attributes) =>
        if (pathManager.project.config.definitions.contains(attributes.target))
          ctx.retc(pathManager.project.config.definitions(attributes.target))
        else {
          scribe.warn(s"unknown name ${attributes.target}" + reporter(attributes.prov))
          ctx.retc(code(attributes.target))
        }

      case mcro @ Macro(Other(othercommand), attributes) =>
        othercommand match {
          case "footnote" =>
            val target = attributes.target
            ctx.retc(a(title := target, "※"))

          case "textsc" =>
            ctx.retc(span(`class` := "smallcaps", attributes.target))

          case tagname @ ("ins" | "del") =>
            ctx.retc(tag(tagname)(attributes.positional.mkString(", ")))

          case protocol @ ("http" | "https" | "ftp" | "irc" | "mailto") =>
            val linktarget = s"$protocol:${attributes.target}"
            ctx.retc(linkTo(attributes, linktarget))

          case "subparagraph" => ctx.retc(b(`class` := "paragraphtitle", attributes.target))

          case "todo" => ctx.retc(code(`class` := "todo", SastToScimConverter().macroToScim(mcro)))

          case "tableofcontents" => ctx.empty

          case other => ctx.retc(unknownMacroOutput(mcro))

        }

      case Macro(Def, _) => ctx.empty

      case mcro @ Macro(Image | Include, attributes) =>
        ctx.retc(unknownMacroOutput(mcro))

    }

  def reportPos(m: Macro): String = reporter(m)

  def unknownMacroOutput(im: Macro): Tag = {
    val str = SastToScimConverter().macroToScim(im)
    scribe.warn(s"unknown macro “$str”" + reportPos(im))
    code(str)
  }

  def linkTo(attributes: Attributes, linktarget: String) = {
    a(href := linktarget)(attributes.positional.headOption.getOrElse[String](linktarget))
  }
}

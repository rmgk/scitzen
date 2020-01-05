package scitzen.outputs

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import better.files.File
import cats.data.Chain
import cats.implicits._
import scalatags.generic.Bundle
import scitzen.generic.RegexContext.regexStringContext
import scitzen.generic.Sast._
import scitzen.generic.{AnalyzedDoc, ConversionContext, HtmlPathManager, ParsedDocument, Reporter, Sast, Scope}
import scitzen.parser.MacroCommand.{Cite, Comment, Def, Fence, Image, Include, Label, Link, Other, Quote, Ref}
import scitzen.parser.{Attributes, Inline, InlineText, Macro, ScitzenDateTime}


class SastToHtmlConverter[Builder, Output <: FragT, FragT]
(val bundle: Bundle[Builder, Output, FragT],
 pathManager: HtmlPathManager,
 bibliography: Map[String, String],
 sdoc: AnalyzedDoc,
 document: Option[ParsedDocument],
 sync: Option[(File, Int)],
 reporter: Reporter,
 preproc: ParsedDocument => SastToSastConverter
) {


  import bundle.all._
  import bundle.tags2.{article, time}


  type CtxCF = ConversionContext[Chain[Frag]]
  type Ctx[T] = ConversionContext[T]
  type Cta = Ctx[_]

  val syncPos =
    document.filter(doc => sync.exists(_._1 == doc.file))
            .fold(Int.MaxValue) { _ => sync.get._2 }

  def listItemToHtml(child: SlistItem)(implicit ctx: Cta): CtxCF = {
    convertSeq(child.content)
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

  def sectionRef(s: Section): String = s.attributes.named.getOrElse("label", s.title.str)

  def convertSeq(b: Seq[Sast])(implicit ctx: Cta): CtxCF = ctx.fold(b)((ctx, sast) => convertSingle(sast)(ctx))
  def convertSingle(b: Sast)(implicit ctx: Cta): CtxCF = {
    b match {


      case sec @ Section(title, subsections, _) =>
        val inner = (if (ctx.scope.level == 1) Chain(tMeta()) else Chain.nil) ++:[Frag]
                    ctx.incScope(convertSeq(subsections)(_))
        inlineValuesToHTML(title.inline)(inner).map { innerFrags =>
          tag("h" + ctx.scope.level)(id := sectionRef(sec), innerFrags.toList) +: inner.data
        }

      case Slist(Nil)      => ctx.empty
      case Slist(children) =>
        if (children.head.marker.contains(":")) {
          ctx.fold[SlistItem, Frag](children) { (ctx, c) =>
            listItemToHtml(c)(ctx).map { innerFrags =>
              Chain(dt(strong(c.marker.replaceAllLiterally(":", ""))), dd(innerFrags.toList))
            }
          }.map(i => Chain(dl(i.toList)))
        }
        else {
          val listTag = if (children.head.marker.contains(".")) ol else ul
          ctx.fold[SlistItem, Frag](children) { (ctx, c) =>
            listItemToHtml(c)(ctx).map(i => Chain(li(i.toList)))
          }.map(i => Chain(listTag(i.toList)))
        }

      case SMacro(mcro) => mcro match {
        case Macro(Image, attributes) =>
          pathManager.project.resolve(pathManager.cwd, attributes.target) match {
            case Some(target) =>
              val path = pathManager.relativizeImage(target)
              ctx.requireInOutput(target, path).retc(img(src := path.toString))
            case None         =>
              scribe.warn(s"could not find path ${attributes.target} in ${pathManager.cwd} and ${document.get.file}")
              ctx.empty
          }

        case Macro(Other("horizontal-rule"), attributes) =>
          ctx.ret(Chain(hr))

        case Macro(Ref, attributes) if attributes.named.get("type").contains("article") =>
          val foundDoc = pathManager.findDoc(attributes.target)
          val post     = foundDoc.get

          def timeShort(date: Option[ScitzenDateTime]) = date match {
            case Some(date) => time(stringFrag(date.monthDayTime + " "))
            case None       => time("00-00 00:00")
          }

          val aref = pathManager.relativePostTarget(post.parsed.file).toString

          ctx.ret(Chain(a(
            href := aref,
            article(timeShort(post.analyzed.date),
                    span(cls := "title", post.analyzed.title),
                    categoriesSpan(post.analyzed)
                    ))))

        case Macro(Include, attributes) =>
          pathManager.findDoc(attributes.target) match {
            case Some(doc) =>
              ctx.withScope(new Scope(3)) { ctx =>
                val resctx = preproc(doc.parsed).convertSeq(doc.sast)(ctx)
                new SastToHtmlConverter(bundle,
                                        pathManager.changeWorkingFile(doc.parsed.file),
                                        bibliography,
                                        doc.analyzed,
                                        Some(doc.parsed),
                                        sync,
                                        doc.parsed.reporter,
                                        preproc)
                .convertSeq(resctx.data.toList)(resctx)
              }
            case None      =>
              scribe.error(s"unknown include ${attributes.target}" + reporter(attributes.prov))
              ctx.empty
          }

        case Macro(Fence, attributes) =>
          pathManager.project.resolve(pathManager.cwd, attributes.target) match {
            case None       => inlineValuesToHTML(List(mcro))
            case Some(file) =>
              convertSingle(SBlock(attributes, Fenced(file.contentAsString)))
          }

        case other =>
          inlineValuesToHTML(List(other))
      }

      case tLBlock: SBlock =>
        val positiontype = tLBlock.attr.positional.headOption
        positiontype match {
          case Some("quote") =>
            convertBlock(tLBlock).map { innerHtml =>
              // for blockquote layout, see example 12 (the twitter quote)
              // http://w3c.github.io/html/textlevel-semantics.html#the-cite-element
              val bq    = blockquote(innerHtml.toList)
              // first argument is "quote" we concat the rest and treat them as a single entity
              val title = tLBlock.attr.positional.drop(1)
              Chain(if (title.nonEmpty) bq(cite(title)) else bq)
            }
          case _             =>
            val prov = tLBlock.attr.prov
            convertBlock(tLBlock).map { html =>
              if (prov.start <= syncPos && syncPos <= prov.end) {
                scribe.info(s"highlighting $syncPos: $prov")
                div(id := "highlight") +: html
              } else html
            }
        }

    }
  }


  def convertBlock(sBlock: SBlock)(implicit ctx: Cta): CtxCF = sBlock.content match {

    case Paragraph(text) => inlineValuesToHTML(text.inline).map(cf => Chain(p(cf.toList)))

    case Parsed(delimiter, blockContent) =>
      delimiter match {
        case rex"=+" => convertSeq(blockContent).map { cf =>
          Chain {
            val fig = figure(cf.toList)
            sBlock.attr.named.get("label").fold(fig: Tag)(l => fig(id := l))
          }
        }
        // space indented blocks are currently only used for description lists
        // they are parsed and inserted as if the indentation was not present
        case rex"\s+" => convertSeq(blockContent)
        case _        => convertSeq(blockContent).map { inner =>
          Chain(div(delimiter, br, inner.toList, br, delimiter))
        }
      }

    case Fenced(text) => sBlock.attr.positional.headOption match {
      // Preformatted plaintext, preserve linebreaks,
      // but also wrap for linebreaks
      case Some("text") => ctx.retc(pre(text))
      // Code listing
      // Use this for monospace, space preserving, line preserving text
      // It may wrap to fit the screen content
      case other =>
        ctx.retc(pre(code(text)))
    }

    case SpaceComment(content) => ctx.empty
  }


  def inlineValuesToHTML(inlines: Seq[Inline])(implicit ctx: Cta): CtxCF =
    ctx.fold(inlines) { (ctx, inline) => inlineToHTML(inline)(ctx) }

  def inlineToHTML(inline: Inline)(implicit ctx: Cta): CtxCF = inline match {
    case InlineText(str) => ctx.retc(stringFrag(str))

    case Macro(Quote(q), attrs) => {
      val inner = attrs.target
      //scribe.warn(s"inline quote $q: $inner; ${post.sourcePath}")
      q.head match {
        case '_' => ctx.retc(em(inner))
        case '*' => ctx.retc(strong(inner))
        case '`' => ctx.retc(code(inner))
        case '$' =>

          val katexdefs      = sdoc.named.get("katexTemplate")
                                   .flatMap(path => pathManager.project.resolve(pathManager.cwd, path))
                                   .map(_.contentAsString).getOrElse("")
          val (mathml, ictx) = ctx.katex(inner, {
            (scala.sys.process.Process(s"katex") #< new ByteArrayInputStream((katexdefs + inner).getBytes(StandardCharsets.UTF_8))).!!
          })
          ictx.retc(span(raw(mathml)))
      }
    }

    case Macro(Comment, attributes) => ctx.empty

    case mcro @ Macro(Cite, attributes) =>
      val anchors = attributes.positional.flatMap {_.split(",")}.map { bibid =>
        bibid -> bibliography.getOrElse(bibid.trim, {
          scribe.error(s"bib key not found: $bibid " + reportPos(mcro))
          bibid
        })
      }.sortBy(_._2).map { case (bibid, bib) => a(href := s"#$bibid", bib) }
      ctx.retc(frag("\u00A0", anchors))


    case Macro(Label, _) => ctx.empty

    case Macro(Link, attributes) =>
      val target = attributes.target
      ctx.retc(linkTo(attributes, target))

    case Macro(Ref, attributes) =>
      pathManager.findDoc(attributes.target) match {
        case Some(fd) =>
          val targetpath = pathManager.relativePostTarget(fd.parsed.file).toString
          val name       = if (attributes.positional.length > 1) attributes.positional.head else fd.parsed.file.nameWithoutExtension
          ctx.retc(
            a(href := targetpath, name)
            )

        case None =>
          sdoc.targets.find(_.id == attributes.positional.head).map[CtxCF] { target =>
            target.resolution match {
              case sec @ Section(title, _, _) => inlineValuesToHTML(title.inline).map { inner =>
                Chain(a(href := s"#${sectionRef(sec)}", inner.toList))
              }
              case other                      =>
                scribe.error(s"can not refer to $other")
                ctx.empty
            }
          }.getOrElse(ctx.empty)
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


        case "n" if pathManager.project.documentManager.attributes.contains(attributes.target) =>
          ctx.retc(pathManager.project.documentManager.attributes(attributes.target))

        case "subparagraph" => ctx.retc(h6(attributes.target))

        case "todo" => ctx.retc(code(`class` := "todo", SastToScimConverter().macroToScim(mcro)))

        case other => ctx.retc(unknownMacroOutput(mcro))

      }

    case mcro @ Macro(Def | Fence | Image | Include, attributes) =>
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

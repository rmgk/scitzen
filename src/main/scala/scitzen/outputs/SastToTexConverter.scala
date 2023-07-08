package scitzen.outputs

import de.rmgk.Chain
import scitzen.cli.ConversionAnalysis
import scitzen.contexts.ConversionContext
import scitzen.extern.ImageTarget
import scitzen.generic.{ArticleRef, Flags, References, SastRef}
import scitzen.sast.DCommand.*
import scitzen.sast.*
import scitzen.outputs.SastToTexConverter.latexencode
import scitzen.compat.Logging.cli
import scitzen.sast.Attribute.Named

object SastToTexConverter {
  def latexencode(input: String): String =
    // replace `\` with dummy first, because `\textbackslash{}` requires the `{}`, which would be replaced in the next step
    val dummyForBSreplace = '\u0011'
    val nobs              = input.replace('\\', dummyForBSreplace)
    val nosimple          = nobs.replaceAll("""([&%$#_{}])""", "\\\\$1")
    List(
      "~"                        -> "\\textasciitilde{}",
      "^"                        -> "\\textasciicircum{}",
      "`"                        -> "\\textasciigrave{}",
      dummyForBSreplace.toString -> "\\textbackslash{}"
    ).foldLeft(nosimple) {
      case (acc, (char, rep)) =>
        acc.replace(char, rep)
    }
}

class SastToTexConverter(
    articleRef: ArticleRef,
    anal: ConversionAnalysis,
    settings: Attributes,
    flags: Flags,
) extends ProtoConverter[String, String](articleRef, anal, settings):

  type CtxCS  = ConversionContext[Chain[String]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[?]

  override def subconverter(
      articleRef: ArticleRef,
      analysis: ConversionAnalysis,
      attr: Attributes
  ): ProtoConverter[String, String] =
    new SastToTexConverter(articleRef, analysis, attr, flags)

  override def stringToInlineRes(str: String): String = latexencode(str)

  val sectioning: Int => String = nesting => {
    // "book", "part", "chapter",
    val secs = settings.plain("sectioning")
      .getOrElse("chapter,section,subsection,paragraph").split(',').map(_.trim)
    val sec = secs.lift(nesting).getOrElse("paragraph")
    sec
  }

  override def inlineResToBlock(inl: Chain[String]): String  = inl.mkString("")
  override def inlinesAsToplevel(inl: Chain[String]): String = inl.mkString("", "", "\n")

  override def convertSection(ctx: Cta, section: Section): CtxCF =
    val Section(title, prefix, attr) = section
    val ilc                          = convertInlineSeq(ctx, title.inl).map(inlineResToBlock)

    if prefix == "="
    then
      val author = section.attributes.plain("author").fold("")(n => s"\\author{${latexencode(n)}}")
      ilc.retc(s"\\title{${ilc.data}}$author\\scitzenmaketitle{}")
    else
      val pushed = ilc.push(section)
      val numbered = if attr.plain("style").contains("unnumbered") then "*"
      else ""
      val header =
        val shift = 1 - pushed.sections.collectFirst { case Section(_, "==", _) => () }.size
        val sec   = sectioning(section.level - shift)
        // not entirely sure what the following does
        val chapterAdd = if section.level == 0 then s"[${ilc.data}]"
        else ""
        s"\\$sec$numbered$chapterAdd{${ilc.data}}"

      val label = attr.plain("unique ref").map(l => s"\\label{$l}").toList
      pushed.retc(header) :++ Chain.from(label)

  override def convertSlist(ctx: Cta, slist: Slist): CtxCF =
    val children = slist.children
    children match
      case Nil => ctx.ret(Chain.nil)

      case ListItem(marker, _, None | Some(Slist(_))) :: _ =>
        val listType = if marker.contains(".") then "enumerate"
        else "itemize"
        s"\\begin{$listType}" +:
        ctx.fold[ListItem, String](children) { (ctx, child) =>
          val inlineCtx = convertInlinesCombined(ctx, child.text.inl).map(s => Chain(s"\\item{$s}"))
          val contentCtx =
            child.content.fold(inlineCtx.empty[String])((singleSast: Sast) => convertSast(inlineCtx, singleSast))
          inlineCtx.data ++: contentCtx
        } :+
        s"\\end{$listType}"

      case ListItem(_, _, _) :: _ =>
        ctx.fold[ListItem, String](children) { (ctx, child) =>
          val inlinesCtx = convertInlinesCombined(ctx, child.text.inl).map(s => s"\\item[$s]{}")
          inlinesCtx.data +: child.content.fold(inlinesCtx.empty[String])((singleSast: Sast) =>
            convertSast(inlinesCtx, singleSast)
          )
        }.map { content =>
          "\\begin{description}" +: content :+ "\\end{description}"
        }

  override def convertBlockDirective(ctx: Cta, directive: Directive): CtxCF =
    directive.command match
      case Include            => handleInclude(ctx, directive)
      case Other("aggregate") => handleAggregate(ctx, directive)

      case other =>
        convertInlineDirective(ctx, directive).mapc(inlineResToBlock)

  def texbox(name: String, attributes: Attributes, content: Seq[Sast])(ctx: Cta): CtxCS =
    val target    = attributes.target
    val optionals = if target.isEmpty then "" else s"[$target]"
    val label     = attributes.plain("unique ref").map(s => s"\\label{$s}").getOrElse("")
    s"\\begin{$name}$optionals$label" +:
    convertSastSeq(ctx, content) :+
    s"\\end{$name}"

  override def convertBlock(ctx: Cta, block: Block): CtxCS =
    val innerCtx: CtxCS =
      block.content match
        case Paragraph(content) =>
          val cctx = convertInlinesCombined(ctx, content.inl)
          // appending the newline adds two newlines in the source code to separate the paragraph from the following text
          // the latexenc text does not have any newlines at the end because of the .trim
          if !flags.hardwrap then cctx.single :+ ""
          else
            cctx.map { text =>
              val latexenc = text.trim.replace("\n", "\\newline{}\n")
              Chain("\\noindent", latexenc, "\n")
            }

        case Parsed(_, blockContent) =>
          block.command match
            case BCommand.Figure =>
              val (figContent, caption) =
                blockContent.lastOption match
                  case Some(Block(_, _, Paragraph(content))) =>
                    val captionstr = convertInlinesCombined(ctx, content.inl)
                    (blockContent.init, captionstr.map(str => s"\\caption{$str}"))
                  case _ =>
                    cli.warn(s"figure has no caption" + doc.reporter(block.prov))
                    (blockContent, ctx.ret(""))
              "\\begin{figure}" +:
              "\\centerfloat" +:
              convertSastSeq(caption, figContent) :++
              Chain(
                caption.data,
                block.attributes.plain("unique ref").fold("")(l => s"\\label{$l}"),
                "\\end{figure}"
              )

            case BCommand.Other(name @ ("theorem" | "definition" | "proofbox" | "proof" | "lemma" | "example")) =>
              texbox(name, block.attributes, blockContent)(ctx).useFeature("framed")

            case BCommand.Other(name @ "abstract") => texbox(name, block.attributes, blockContent)(ctx)

            case _ =>
              convertSastSeq(ctx, blockContent)

        case Fenced(text) =>
          val labeltext = block.attributes.plain("unique ref") match
            case None => text
            case Some(label) =>
              text.replaceAll(""":§([^§]*?)§""", s"""(*@\\\\label{$label$$1}@*)""")
          val restext =
            if block.attributes.target == "highlight" then labeltext
            else
              labeltext.replaceAll(""":hl§([^§]*?)§""", s"""(*@\\\\textbf{$$1}@*)""")
          if block.command == BCommand.Other("text")
          then ctx.ret(Chain(s"\\begin{verbatim}", restext, "\\end{verbatim}"))
          else ctx.ret(Chain(s"\\begin{lstlisting}", restext, "\\end{lstlisting}")).useFeature("listings")

        case SpaceComment(_) => ctx.empty

    if !flags.notes then innerCtx
    else
      block.attributes.get("note").fold(innerCtx) { note =>
        convertInlinesCombined(innerCtx, note.text.inl).map { (content: String) =>
          s"\\sidepar{$content}%" +: innerCtx.data
        }.useFeature("sidepar")
      }

  def nbrs(attributes: Attributes)(ctx: Cta): Ctx[String] =
    attributes.textOption match
      case None => ctx.ret("")
      case Some(arg) =>
        convertInlinesCombined(ctx, arg.inl).map { str =>
          s"${str}~"
        }

  override def convertInlineText(ctx: Cta, inlineText: InlineText): CtxCF = ctx.retc(latexencode(inlineText.str))

  override def convertInlineDirective(ctx: Cta, directive: Directive): CtxCF =
    val attributes = directive.attributes
    directive.command match
      case Code    => ctx.retc(s"\\texttt{${latexencode(attributes.target)}}")
      case Comment => ctx.retc("")
      case Def     => ctx.retc("")
      case Emph    => convertInlinesCombined(ctx, attributes.text.inl).mapc((str: String) => s"\\emph{$str}")
      case Math =>
        val math = attributes.target
        if math.isBlank then
          cli.warn("empty math", directive)
          ctx.ret(Chain.empty)
        else
          ctx.retc(s"$$${attributes.target}$$")
      case Other("break") => ctx.retc(s"\\clearpage{}")
      case Other("rule") => convertBlockDirective(
          ctx,
          Directive(
            Ref,
            Attributes(
              Seq(
                Named("", Text(Seq(Directive(Other("smallcaps"), attributes)(directive.prov)))),
                Attribute("style", "plain"),
                Attribute("", s"rule-${attributes.target}")
              )
            )
          )(
            directive.prov
          )
        )
      case Other("smallcaps") => ctx.retc(s"\\textsc{${attributes.target}}")
      case Raw                => ctx.retc(attributes.plain("tex").getOrElse(""))
      case Other("todo") =>
        convertInlinesCombined(ctx, attributes.text.inl).mapc(str => s"{\\color{red}TODO:${str}}")
      case Strong => convertInlinesCombined(ctx, attributes.text.inl).mapc(str => s"\\textbf{$str}")
      case Other("partition") =>
        convertInlinesCombined(ctx, attributes.text.inl).mapc(str => s"\\part{${str}}")

      case BibQuery => convertInline(ctx, anal.bib.convert(directive))
      case Cite =>
        val cmndCtx = attributes.plain("style") match
          case Some("author") => ctx.ret("citet")
          case Some("inline") => ctx.ret("bibentry").useFeature("bibentry")
          case _              => ctx.ret("cite")

        nbrs(attributes)(cmndCtx).mapc(str => s"$str\\${cmndCtx.data}{${attributes.target}}")

      case Ref =>
        if attributes.get("scope").isDefined then
          cli.warn(s"scope support unclear", directive)
          ()
        val candidates =
          References.filterCandidates(
            doc.path,
            anal.directory.labels.getOrElse(attributes.target, Nil)
          )

        if candidates.sizeIs > 1 then
          cli.warn(
            s"multiple resolutions for ${attributes.target}",
            directive
          )
          cli.warn(
            s"\tresolutions are in: ${candidates.map(c => c.scope).mkString("\n\t", "\n\t", "\n\t")}"
          )

        candidates.headOption match
          case None =>
            cli.warn(s"no resolution found for ${attributes.target}", directive)
            ctx.empty
          case Some(candidate) =>
            // TODO: existence of line is unchecked
            val label = References.getLabel(candidate).get + attributes.plain("line").getOrElse("")
            attributes.plain("style") match
              case Some("plain") =>
                convertInlinesCombined(ctx, attributes.text.inl).mapc { str =>
                  s"\\hyperref[${label}]{${str}}"
                }
              case _ => nbrs(attributes)(ctx).mapc { str => s"${str}\\ref{${label}}" }

      case Link =>
        val target   = latexencode(attributes.target)
        val plainurl = s"\\url{$target}"
        attributes.textOption match
          case Some(text) =>
            convertInlinesCombined(ctx, text.inl).mapc: res =>
              val name    = "{" + res + "}"
              val textref = s"\\href{$target}{$name}"
              if settings.plain("footnotelinks").contains("disabled") then textref
              else s"$textref\\footnote{$plainurl}"
            .useFeature("href")
          case None =>
            ctx.retc {
              plainurl
            }

      case Lookup =>
        handleLookup(directive) match
          case Some(res) =>
            convertInlinesCombined(ctx, res.inl).single
          case None =>
            ctx.retc(latexencode(attributes.target))

      case Other("footnote") =>
        convertInlinesCombined(ctx, attributes.text.inl).map(target => s"\\footnote{$target}").single

      case Other(_) =>
        cli.warn(s"unknown macro", directive)
        ctx.retc(stringToInlineRes(directiveString(directive)))

      case Image =>
        convertImage(ctx, directive, ImageTarget.Tex): target =>
          if target.absolute.toString.endsWith(".tex") then
            cli.warn("tex image no longer supported", directive)
          val mw = java.lang.Double.parseDouble(attributes.plain("maxwidth").getOrElse("1"))
          ctx.retc(s"\\includegraphics[max width=$mw\\columnwidth]{${target.absolute}}").useFeature(
            "graphics"
          )

      case Include | Script | Aggregate | Index =>
        cli.warn(s"not supported as inline tex", directive)
        ctx.retc(stringToInlineRes(directiveString(directive)))

end SastToTexConverter

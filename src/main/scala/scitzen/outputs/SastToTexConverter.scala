package scitzen.outputs

import de.rmgk.Chain
import scitzen.cli.ConversionAnalysis
import scitzen.compat.Logging.cli
import scitzen.contexts.ConversionContext
import scitzen.project.{ArticleRef, Flags, References, SastRef}
import scitzen.outputs.SastToTexConverter.latexencode
import scitzen.resources.ImageTarget
import scitzen.sast.*
import scitzen.sast.DCommand.*

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
    articleRef: ::[ArticleRef],
    anal: ConversionAnalysis,
    settings: Attributes,
    flags: Flags,
) extends ProtoConverter[String, String](articleRef, anal, settings):

  type CtxCS  = ConversionContext[Chain[String]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[?]

  override def subconverter(
      aref: ArticleRef,
      attr: Attributes
  ): ProtoConverter[String, String] =
    new SastToTexConverter(::(aref, articleRef), anal, attr, flags)

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
    val Section(title, prefix, attr, _) = section
    val ilc                             = convertInlineSeq(ctx, title.inl).map(inlineResToBlock)

    if prefix == "="
    then
      val author = section.attributes.plain("author").fold("")(n => s"\\author{${latexencode(n)}}")
      ilc.retc(s"\\title{${ilc.data}}$author\\scitzenmaketitle{}")
    else
      val pushed = ilc.push(section)
      val numbered = if attr.plain("style").contains("unnumbered") then "*"
      else ""
      val header =
        val shift = 1 - pushed.sections.collectFirst { case Section(_, "==", _, _) => () }.size
        val sec   = sectioning(section.level - shift)
        // not entirely sure what the following does
        val chapterAdd = if section.level == 0 then s"[${ilc.data}]"
        else ""
        s"\\$sec$numbered$chapterAdd{${ilc.data}}"

      val label = attr.plain("unique ref").map(l => s"\\label{$l}").toList
      pushed.retc(header) :++ Chain.from(label)

  override def convertDefinitionList(ctx: Cta, deflist: FusedDefinitions): CtxCF = {
    ctx.fold[FusedDefinitionItem, String](deflist.items): (ctx, item) =>
      val text  = convertInlineSeq(ctx, item.text.inl)
      val inner = convertSastSeq(text, item.content)
      inner.ret(
        s"\\item[${text.data}]{}\\hfill{}" +: inner.data
      )
    .map: content =>
      "\\begin{description}" +: content :+ "\\end{description}"
  }

  override def convertSlist(ctx: Cta, slist: FusedList): CtxCF = {
    if slist.items.isEmpty then return ctx.empty
    val recipes = ctx.fold[FusedListItem, String](slist.items):
      case (ctx, item) =>
        val para  = convertInlinesCombined(ctx, item.inlines)
        val inner = convertSlist(para, FusedList(item.children))
        inner.ret(
          s"\\item{${para.data}}" +: inner.data
        )

    recipes.map: i =>
      val listType =
        if slist.items.headOption.map(_.marker.contains(".")).getOrElse(false)
        then "enumerate"
        else "itemize"
      s"\\begin{$listType}" +: i :+ s"\\end{$listType}"
  }

  override def convertBlockDirective(ctx: Cta, directive: Directive): CtxCF =
    directive.command match
      case Include            => handleInclude(ctx, directive)
      case Other("aggregate") => handleAggregate(ctx, directive)
      case Image =>
        "\\noindent" +: convertInlineDirective(ctx, directive)

      case other =>
        convertInlineDirective(ctx, directive).mapc(inlineResToBlock)

  def texbox(name: String, attributes: Attributes, content: Seq[Sast])(ctx: Cta): CtxCS =
    val target    = attributes.target
    val optionals = if target.isEmpty then "" else s"[$target]"
    val label     = attributes.plain("unique ref").map(s => s"\\label{$s}").getOrElse("")
    s"\\begin{$name}$optionals$label" +:
    convertSastSeq(ctx, content) :+
    s"\\end{$name}"

  override def convertParagraph(ctx: Cta, paragraph: Paragraph): CtxCF =
    val cctx = convertInlinesCombined(ctx, paragraph.inlines)
    // appending the newline adds two newlines in the source code to separate the paragraph from the following text
    // the latexenc text does not have any newlines at the end because of the .trim
    if flags.hardwrap then
      cctx.map { text =>
        val latexenc = text.strip().replace("\n", "\\newline{}\n")
        Chain("\n\\noindent", latexenc, "")
      }
    else cctx.map(c => Chain("", c, ""))

  override def convertDelimited(ctx: Cta, block: FusedDelimited): CtxCS = {
    val innerCtx: CtxCS =
      val blockContent = block.content
      block.delimiter.command match
        case BCommand.Figure =>
          val (figContent, caption) =
            blockContent.lastOption match
              case Some(paragraph: Paragraph) =>
                val captionstr = convertInlinesCombined(ctx, paragraph.inlines)
                (blockContent.init, captionstr.map(str => s"\\caption{$str}"))
              case _ =>
                cli.warn(s"figure has no caption" + doc.reporter(block.delimiter.meta.prov))
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

    if !flags.notes then innerCtx
    else
      block.attributes.get("note").fold(innerCtx) { note =>
        convertInlinesCombined(innerCtx, note.text.inl).map { (content: String) =>
          s"\\sidepar{$content}%" +: innerCtx.data
        }.useFeature("sidepar")
      }
  }

  override def convertFenced(ctx: Cta, block: Fenced): CtxCS = {
    val innerCtx: CtxCS =
      val text = block.content
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

    if !flags.notes then innerCtx
    else
      block.attributes.get("note").fold(innerCtx) { note =>
        convertInlinesCombined(innerCtx, note.text.inl).map { (content: String) =>
          s"\\sidepar{$content}%" +: innerCtx.data
        }.useFeature("sidepar")
      }
  }

  def nbrs(attributes: Attributes)(ctx: Cta): Ctx[String] =
    attributes.description match
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
      case Other("rule") => convertInlineDirective(
          ctx,
          Directive(
            Ref,
            Attributes(
              Seq(
                Attribute("", "", Text(Seq(Directive(Other("smallcaps"), attributes, directive.meta)))),
                Attribute("style", "plain"),
                Attribute("", s"rule-${attributes.target}")
              )
            ),
            directive.meta
          )
        )
      case Other("smallcaps") => ctx.retc(s"\\textsc{${attributes.target}}")
      case Raw                => ctx.retc(attributes.plain("tex").getOrElse(""))
      case Other("todo") =>
        convertInlinesCombined(ctx, attributes.text.inl).mapc(str => s"{\\color{red}TODO:${str}}")
      case Strong => convertInlinesCombined(ctx, attributes.text.inl).mapc(str => s"\\textbf{$str}")
      case Other("partition") =>
        convertInlinesCombined(ctx, attributes.text.inl).mapc(str => s"\\part{${str}}")

      case BibQuery => convertInline(ctx, anal.bib.convertBibQuery(directive))
      case Cite =>
        val usedctx = ctx.cite(
          attributes.target.split(',').iterator.map(_.trim).filterNot(_.isEmpty).flatMap(anal.bib.entries.get).toList
        )
        val cmndCtx = attributes.plain("style") match
          case Some("author") => usedctx.ret("citet")
          case Some("inline") => usedctx.ret("bibentry").useFeature("bibentry")
          case _              => usedctx.ret("cite")

        nbrs(attributes)(cmndCtx).mapc(str => s"$str\\${cmndCtx.data}{${attributes.target}}")

      case Ref =>
        if attributes.get("scope").isDefined then
          cli.warn(s"scope support unclear", directive)
          ()
        val candidates =
          References.filterCandidates(
            doc.path,
            anal.directory.labels.getOrElse(attributes.target, Nil),
            _.scope.absolute
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
        attributes.description match
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
        convertImage(ctx, directive, ImageTarget.Tex): ctx =>
          if ctx.data.file.absolute.toString.endsWith(".tex") then
            cli.warn("tex image no longer supported", directive)
          val mw = java.lang.Double.parseDouble(attributes.plain("maxwidth").getOrElse("1"))
          ctx.retc(s"\\includegraphics[max width=$mw\\columnwidth]{${ctx.data.relativeFinalization}}").useFeature(
            "graphics"
          )

      case Include | Script | Aggregate | Index =>
        cli.warn(s"not supported as inline tex", directive)
        ctx.retc(stringToInlineRes(directiveString(directive)))

end SastToTexConverter

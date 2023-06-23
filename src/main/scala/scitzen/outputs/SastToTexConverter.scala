package scitzen.outputs

import de.rmgk.Chain
import scitzen.cli.ConversionAnalysis
import scitzen.contexts.ConversionContext
import scitzen.extern.ImageTarget
import scitzen.generic.{Document, References, SastRef, TitledArticle}
import scitzen.sast.DCommand.*
import scitzen.sast.*
import scitzen.outputs.SastToTexConverter.latexencode
import scitzen.sast.Attribute.{Plain, Positional}
import scitzen.compat.Logging.scribe

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
    doc: Document,
    anal: ConversionAnalysis,
    settings: Attributes
) extends ProtoConverter[String, String](doc, anal, settings):

  type CtxCS  = ConversionContext[Chain[String]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[?]

  override def subconverter(
      doc: Document,
      analysis: ConversionAnalysis,
      attr: Attributes
  ): ProtoConverter[String, String] =
    new SastToTexConverter(doc, analysis, settings)

  override def stringToInlineRes(str: String): String = latexencode(str)

  def articleHeader(article: TitledArticle, cta: Cta): CtxCS =
    val hasToc = cta.features.contains("tableofcontents")
    val fm     = if hasToc then Chain("\\frontmatter") else Chain.empty

    val ilc    = convertInlineSeq(cta, article.header.titleText.inl).map(inlineResToBlock)
    val author = article.header.attributes.named.get("author").fold("")(n => s"\\author{${latexencode(n)}}")
    ilc.ret(fm :+ s"\\title{${ilc.data}}$author\\scitzenmaketitle{}")

  val sectioning: Int => String = nesting => {
    // "book", "part", "chapter",
    val secs = settings.named.get("sectioning")
      .getOrElse("chapter,section,subsection,paragraph").split(',').map(_.trim)
    val sec = secs.lift(nesting).getOrElse("paragraph")
    sec
  }

  override def inlineResToBlock(inl: Chain[String]): String  = inl.mkString("")
  override def inlinesAsToplevel(inl: Chain[String]): String = inl.mkString("", "", "\n")

  override def convertSection(ctx: Cta, section: Section): CtxCF =
    val Section(title, prefix, attr) = section
    val ilc                          = convertInlineSeq(ctx, title.inl).map(inlineResToBlock)

    val pushed = ilc.push(section)
    val numbered = if attr.named.get("style").contains("unnumbered") then "*"
    else ""
    val header =
      val shift = 1 - pushed.sections.collectFirst { case Section(_, "==", _) => () }.size
      val sec   = sectioning(section.level - shift)
      // not entirely sure what the following does
      val chapterAdd = if section.level == 0 then s"[${ilc.data}]"
      else ""
      s"\\$sec$numbered$chapterAdd{${ilc.data}}"

    val label = attr.named.get("unique ref").map(l => s"\\label{$l}").toList
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
          val inlineCtx  = convertInlinesAsBlock(ctx, child.text.inl).map(s => Chain(s"\\item{$s}"))
          val contentCtx = child.content.fold(inlineCtx.empty[String])((singleSast: Sast) => convertSast(inlineCtx, singleSast))
          inlineCtx.data ++: contentCtx
        } :+
        s"\\end{$listType}"

      case ListItem(_, _, _) :: _ =>
        ctx.fold[ListItem, String](children) { (ctx, child) =>
          val inlinesCtx = convertInlinesAsBlock(ctx, child.text.inl).map(s => s"\\item[$s]{}")
          inlinesCtx.data +: child.content.fold(inlinesCtx.empty[String])((singleSast: Sast) => convertSast(inlinesCtx, singleSast))
        }.map { content =>
          "\\begin{description}" +: content :+ "\\end{description}"
        }

  override def convertBlockDirective(ctx: Cta, directive: Directive): CtxCF =
    directive.command match
      case Include            => handleInclude(ctx, directive)
      case Other("aggregate") => handleAggregate(ctx, directive)

      case other =>
        convertDirective(ctx, directive).mapc(inlineResToBlock)

  def texbox(name: String, attributes: Attributes, content: Seq[Sast])(ctx: Cta): CtxCS =
    val args      = attributes.legacyPositional
    val optionals = if args.isEmpty then "" else args.mkString("[", "; ", "]")
    val label     = attributes.named.get("unique ref").map(s => s"\\label{$s}").getOrElse("")
    s"\\begin{$name}$optionals$label" +:
    convertSastSeq(ctx, content) :+
    s"\\end{$name}"

  override def convertBlock(ctx: Cta, block: Block): CtxCS =
    val innerCtx: CtxCS =
      block.content match
        case Paragraph(content) =>
          val cctx = convertInlinesAsBlock(ctx, content.inl)
          // appending the newline adds two newlines in the source code to separate the paragraph from the following text
          // the latexenc text does not have any newlines at the end because of the .trim
          if !hardNewlines then cctx.single :+ ""
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
                    val captionstr = convertInlinesAsBlock(ctx, content.inl)
                    (blockContent.init, captionstr.map(str => s"\\caption{$str}"))
                  case _ =>
                    scribe.warn(s"figure has no caption" + doc.reporter(block.prov))
                    (blockContent, ctx.ret(""))
              "\\begin{figure}" +:
              "\\centerfloat" +:
              convertSastSeq(caption, figContent) :++
              Chain(
                caption.data,
                block.attributes.named.get("unique ref").fold("")(l => s"\\label{$l}"),
                "\\end{figure}"
              )

            case BCommand.Other(name @ ("theorem" | "definition" | "proofbox" | "proof" | "lemma" | "example")) =>
              texbox(name, block.attributes, blockContent)(ctx).useFeature("framed")

            case BCommand.Other(name @ "abstract") => texbox(name, block.attributes, blockContent)(ctx)

            case _ =>
              convertSastSeq(ctx, blockContent)

        case Fenced(text) =>
          val labeltext = block.attributes.named.get("unique ref") match
            case None => text
            case Some(label) =>
              text.replaceAll(""":§([^§]*?)§""", s"""(*@\\\\label{$label$$1}@*)""")
          val restext =
            if !block.attributes.legacyPositional.contains("highlight") then labeltext
            else
              labeltext.replaceAll(""":hl§([^§]*?)§""", s"""(*@\\\\textbf{$$1}@*)""")
          if block.command == BCommand.Other("text")
          then ctx.ret(Chain(s"\\begin{verbatim}", restext, "\\end{verbatim}"))
          else ctx.ret(Chain(s"\\begin{lstlisting}", restext, "\\end{lstlisting}")).useFeature("listings")

        case SpaceComment(_) => ctx.empty

    if project.config.notes.contains("hide") then innerCtx
    else
      block.attributes.nestedMap.get("note").fold(innerCtx) { note =>
        convertInlinesAsBlock(innerCtx, note.targetT.inl).map { (content: String) =>
          s"\\sidepar{$content}%" +: innerCtx.data
        }.useFeature("sidepar")
      }

  def nbrs(attributes: Attributes)(ctx: Cta): Ctx[String] =
    attributes.argumentsT match
      case Nil => ctx.ret("")
      case arg :: _ =>
        convertInlinesAsBlock(ctx, attributes.argumentsT.head.inl).map { str =>
          s"${str}~"
        }

  override def convertText(ctx: Cta, inlineText: InlineText): CtxCF = ctx.retc(latexencode(inlineText.str))

  override def convertDirective(ctx: Cta, directive: Directive): CtxCF =
    val attributes = directive.attributes
    directive.command match
      case Code    => ctx.retc(s"\\texttt{${latexencode(attributes.target)}}")
      case Comment => ctx.retc("")
      case Def     => ctx.retc("")
      case Emph    => convertInlinesAsBlock(ctx, attributes.targetT.inl).mapc((str: String) => s"\\emph{$str}")
      case Math =>
        val math = attributes.target
        if math.isBlank then
          warn("empty math", directive)
          ctx.ret(Chain.empty)
        else
          ctx.retc(s"$$${attributes.target}$$")
      case Other("break") => ctx.retc(s"\\clearpage{}")
      case Other("rule") => convertBlockDirective(ctx, Directive(
        Ref,
        Attributes(
          Seq(
            Positional(Text(Seq(Directive(Other("smallcaps"), attributes)(directive.prov))), ""),
            Plain("style", "plain"),
            Positional(s"rule-${attributes.target}")
          )
        )
      )(
        directive.prov
      ))
      case Other("smallcaps") => ctx.retc(s"\\textsc{${attributes.target}}")
      case Raw                => ctx.retc(attributes.named.getOrElse("tex", ""))
      case Other("todo") =>
        convertInlinesAsBlock(ctx, attributes.targetT.inl).mapc(str => s"{\\color{red}TODO:${str}}")
      case Strong => convertInlinesAsBlock(ctx, attributes.targetT.inl).mapc(str => s"\\textbf{$str}")
      case Other("partition") =>
        convertInlinesAsBlock(ctx, attributes.targetT.inl).mapc(str => s"\\part{${str}}")

      case BibQuery => convertInline(ctx, anal.bib.convert(directive))
      case Cite =>
        val cmndCtx = attributes.named.get("style") match
          case Some("author") => ctx.ret("citet")
          case Some("inline") => ctx.ret("bibentry").useFeature("bibentry")
          case _              => ctx.ret("cite")

        nbrs(attributes)(cmndCtx).mapc(str => s"$str\\${cmndCtx.data}{${attributes.target}}")

      case Ref =>
        if attributes.named.contains("scope") then
          warn(s"scope support unclear", directive)
          ()
        val candidates =
          References.filterCandidates(
            doc.path,
            anal.directory.labels.getOrElse(attributes.target, Nil)
          )

        if candidates.sizeIs > 1 then
          scribe.error(
            s"multiple resolutions for ${attributes.target}" +
            doc.reporter(directive) +
            s"\n\tresolutions are in: ${candidates.map(c => c.scope).mkString("\n\t", "\n\t", "\n\t")}"
          )

        candidates.headOption match
          case None =>
            scribe.error(s"no resolution found for ${attributes.target}" + doc.reporter(directive))
            ctx.empty
          case Some(candidate) =>
            // TODO: existence of line is unchecked
            val label = References.getLabel(candidate).get + attributes.named.getOrElse("line", "")
            attributes.named.get("style") match
              case Some("plain") =>
                convertInlinesAsBlock(ctx, attributes.argumentsT.head.inl).mapc { str =>
                  s"\\hyperref[${label}]{${str}}"
                }
              case _ => nbrs(attributes)(ctx).mapc { str => s"${str}\\ref{${label}}" }

      case Link =>
        ctx.retc {
          val target   = attributes.target
          val plainurl = s"\\url{$target}"
          if attributes.legacyPositional.size > 1 then
            val name    = "{" + latexencode(attributes.legacyPositional.head) + "}"
            val textref = s"\\href{$target}{$name}"
            if settings.named.get("footnotelinks").contains("disabled") then textref
            else s"$textref\\footnote{$plainurl}"
          else plainurl
        }.useFeature("href")

      case Lookup =>
        handleLookup(directive) match
          case Some(res) =>
            convertInlinesAsBlock(ctx, res.inl).single
          case None =>
            ctx.retc(latexencode(attributes.target))

      case Other("footnote") =>
        convertInlinesAsBlock(ctx, attributes.targetT.inl).map(target => s"\\footnote{$target}").single

      case Other("tableofcontents") =>
        ctx.useFeature("tableofcontents").retc(
          List("\\cleardoublepage", "\\tableofcontents*", "\\mainmatter").mkString("\n")
        )

      case Other(_) =>
        val str = warn(s"unknown macro", directive)
        ctx.retc(str)

      case Image =>
        convertImage(ctx, directive, ImageTarget.Tex): target =>
          if target.absolute.toString.endsWith(".tex") then
            warn("tex image no longer supported", directive)
            ()
          val mw = java.lang.Double.parseDouble(attributes.named.getOrElse("maxwidth", "1"))
          ctx.retc(s"\\includegraphics[max width=$mw\\columnwidth]{${target.absolute}}").useFeature(
            "graphics"
          )

      case Include | Script =>
        val str: String = warn(s"not supported by tex backend", directive)
        ctx.retc(str)

end SastToTexConverter

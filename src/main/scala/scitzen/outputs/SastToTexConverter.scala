package scitzen.outputs

import de.rmgk.Chain
import scitzen.bibliography.BibDB
import scitzen.contexts.ConversionContext
import scitzen.extern.ImageTarget
import scitzen.generic.{Article, ArticleDirectory, Project, References, SastRef, TitledArticle}
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
    project: Project,
    article: Article,
    articleDirectory: ArticleDirectory,
    bibDB: BibDB,
):

  type CtxCS  = ConversionContext[Chain[String]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[?]

  def articleHeader(article: TitledArticle, cta: Cta): CtxCS =
    val hasToc = cta.features.contains("tableofcontents")
    val fm     = if hasToc then Chain("\\frontmatter") else Chain.empty

    val ilc    = inlineValuesToTex(article.header.titleText.inl)(cta)
    val author = article.header.attributes.named.get("author").fold("")(n => s"\\author{${latexencode(n)}}")
    ilc.ret(fm :+ s"\\title{${ilc.data}}$author\\scitzenmaketitle{}")

  def convert(mainSast: List[Sast])(ctx: Cta): CtxCS =
    sastSeqToTex(mainSast)(ctx)

  def sastSeqToTex(b: Seq[Sast])(ctx: Cta): CtxCS =
    ctx.fold(b) { (ctx, sast) => sastToTex(sast)(ctx) }

  val sectioning: Int => String = nesting => {
    // "book", "part", "chapter",
    val secs = project.definitions.get("sectioning").map(_.plainString)
      .getOrElse("chapter,section,subsection,paragraph").split(',').map(_.trim)
    val sec = secs.lift(nesting).getOrElse("paragraph")
    sec
  }

  def sastToTex(sast: Sast)(ctx: Cta): CtxCS =
    sast match
      case tlBlock: Block => blockToTex(tlBlock)(ctx)

      case section @ Section(title, prefix, attr) =>
        val ilc = inlineValuesToTex(title.inl)(ctx)

        val pushed   = ilc.push(section)
        val numbered = if attr.named.get("style").contains("unnumbered") then "*" else ""
        val header =
          val shift = 1 - pushed.sections.collectFirst { case Section(_, "==", _) => () }.size
          val sec   = sectioning(prefix.length - shift)
          // not entirely sure what the following does
          val chapterAdd = if prefix == "==" then s"[${ilc.data}]" else ""
          s"\\$sec$numbered$chapterAdd{${ilc.data}}"

        val label = attr.named.get("unique ref").map(l => s"\\label{$l}").toList

        pushed.retc(header) :++ Chain.from(label)

      case Slist(children) =>
        children match
          case Nil => ctx.ret(Chain.nil)

          case ListItem(marker, _, None | Some(Slist(_))) :: _ =>
            val listType = if marker.contains(".") then "enumerate" else "itemize"
            s"\\begin{$listType}" +:
            ctx.fold[ListItem, String](children) { (ctx, child) =>
              val inlineCtx  = inlineValuesToTex(child.text.inl)(ctx).map(s => Chain(s"\\item{$s}"))
              val contentCtx = child.content.fold(inlineCtx.empty[String])(sastToTex(_)(inlineCtx))
              inlineCtx.data ++: contentCtx
            } :+
            s"\\end{$listType}"

          case ListItem(_, _, _) :: _ =>
            ctx.fold[ListItem, String](children) { (ctx, child) =>
              val inlinesCtx = inlineValuesToTex(child.text.inl)(ctx).map(s => s"\\item[$s]{}")
              inlinesCtx.data +: child.content.fold(inlinesCtx.empty[String])(sastToTex(_)(inlinesCtx))
            }.map { content =>
              "\\begin{description}" +: content :+ "\\end{description}"
            }

      case mcro: Directive =>
        mcro.command match
          case Include =>
            val target = mcro.attributes.target
            if target.endsWith(".scim")
            then
              warn("include by path no longer supported", mcro)
              ctx.empty
            else
              articleDirectory.itemsAndArticlesByLabel.get(target) match
                case Some(art) =>
                  new SastToTexConverter(project, art.article, articleDirectory, bibDB)
                    .sastSeqToTex(art.article.content)(ctx)

                case None =>
                  warn(s"unknown include ${mcro.attributes.target}", mcro)
                  ctx.empty

          case other =>
            inlineValuesToTex(List(mcro))(ctx).single

  def texbox(name: String, attributes: Attributes, content: Seq[Sast])(ctx: Cta): CtxCS =
    val args      = attributes.legacyPositional.tail
    val optionals = if args.isEmpty then "" else args.mkString("[", "; ", "]")
    val label     = attributes.named.get("unique ref").map(s => s"\\label{$s}").getOrElse("")
    s"\\begin{$name}$optionals$label" +:
    sastSeqToTex(content)(ctx) :+
    s"\\end{$name}"

  def blockToTex(tlblock: Block)(ctx: Cta): CtxCS =
    val innerCtx: CtxCS =
      tlblock.content match
        case Paragraph(content) =>
          val cctx = inlineValuesToTex(content.inl)(ctx)
          // appending the newline adds two newlines in the source code to separate the paragraph from the following text
          // the latexenc text does not have any newlines at the end because of the .trim
          if article.settings.get("style").contains("article") then cctx.single :+ "\n"
          else
            cctx.map { text =>
              val latexenc = text.trim.replace("\n", "\\newline{}\n")
              Chain("\\noindent", latexenc, "\n")
            }

        case Parsed(_, blockContent) =>
          tlblock.command match
            case BCommand.Figure =>
              val (figContent, caption) =
                blockContent.lastOption match
                  case Some(Block(_, _, Paragraph(content))) =>
                    val captionstr = inlineValuesToTex(content.inl)(ctx)
                    (blockContent.init, captionstr.map(str => s"\\caption{$str}"))
                  case _ =>
                    scribe.warn(s"figure has no caption" + article.sourceDoc.reporter(tlblock.prov))
                    (blockContent, ctx.ret(""))
              "\\begin{figure}" +:
              "\\centerfloat" +:
              sastSeqToTex(figContent)(caption) :++
              Chain(
                caption.data,
                tlblock.attributes.named.get("unique ref").fold("")(l => s"\\label{$l}"),
                "\\end{figure}"
              )

            case BCommand.Other(name @ ("theorem" | "definition" | "proofbox" | "proof" | "lemma" | "example")) =>
              texbox(name, tlblock.attributes, blockContent)(ctx).useFeature("framed")

            case BCommand.Other(name @"abstract") => texbox(name, tlblock.attributes, blockContent)(ctx)

            case _ =>
              sastSeqToTex(blockContent)(ctx)

        case Fenced(text) =>
          if tlblock.attributes.named.contains(ImageTarget.Tex.name) then
            val target = tlblock.attributes.named(ImageTarget.Tex.name)
            inlineToTex(Directive(
              Image,
              tlblock.attributes.remove(ImageTarget.Tex.name).append(List(Attribute("", target)))
            )(
              tlblock.prov
            ))(ctx)
          else
            tlblock.attributes.legacyPositional.headOption match

              case _ =>
                val labeltext = tlblock.attributes.named.get("unique ref") match
                  case None => text
                  case Some(label) =>
                    text.replaceAll(""":§([^§]*?)§""", s"""(*@\\\\label{$label$$1}@*)""")
                val restext =
                  if !tlblock.attributes.legacyPositional.contains("highlight") then labeltext
                  else
                    labeltext.replaceAll(""":hl§([^§]*?)§""", s"""(*@\\\\textbf{$$1}@*)""")
                ctx.ret(Chain(s"\\begin{lstlisting}", restext, "\\end{lstlisting}")).useFeature("listings")

        case SpaceComment(_) => ctx.empty

    if project.config.notes.contains("hide") then innerCtx
    else
      tlblock.attributes.nested.get("note").fold(innerCtx) { note =>
        inlineValuesToTex(note.targetT.inl)(innerCtx).map { (content: String) =>
          s"\\sidepar{$content}%" +: innerCtx.data
        }.useFeature("sidepar")
      }

  def nbrs(attributes: Attributes)(ctx: Cta): Ctx[String] =
    attributes.argumentsT match
      case Nil => ctx.ret("")
      case arg :: _ =>
        inlineValuesToTex(attributes.argumentsT.head.inl)(ctx).map { str =>
          s"${str}~"
        }

  def inlineValuesToTex(inners: Seq[Inline])(ctx: Cta): Ctx[String] =
    ctx.fold(inners) { (ctx: Ctx[Chain[String]], inline) => inlineToTex(inline)(ctx) }.map(_.toList.mkString(""))

  def inlineToTex(inln: Inline)(ctx: Cta): CtxCS =
    inln match
      case InlineText(str) => ctx.retc(latexencode(str))
      case mcro: Directive =>
        val attributes = mcro.attributes
        mcro.command match
          case Code    => ctx.retc(s"\\texttt{${latexencode(attributes.target)}}")
          case Comment => ctx.retc("")
          case Def     => ctx.retc("")
          case Emph    => inlineValuesToTex(attributes.targetT.inl)(ctx).mapc(str => s"\\emph{$str}")
          case Math =>
            val math = attributes.target
            if math.isBlank then
              warn("empty math", mcro)
              ctx.ret(Chain.empty)
            else
              ctx.retc(s"$$${attributes.target}$$")
          case Other("break") => ctx.retc(s"\\clearpage{}")
          case Other("rule") => inlineToTex(Directive(
              Ref,
              Attributes(
                Seq(
                  Positional(Text(Seq(Directive(Other("smallcaps"), attributes)(mcro.prov))), ""),
                  Plain("style", "plain"),
                  Positional(s"rule-${attributes.target}")
                )
              )
            )(
              mcro.prov
            ))(ctx)
          case Other("smallcaps") => ctx.retc(s"\\textsc{${attributes.target}}")
          case Raw                => ctx.retc(attributes.named.getOrElse("tex", ""))
          case Other("todo") =>
            inlineValuesToTex(attributes.targetT.inl)(ctx).mapc(str => s"{\\color{red}TODO:${str}}")
          case Strong => inlineValuesToTex(attributes.targetT.inl)(ctx).mapc(str => s"\\textbf{$str}")
          case Other("partition") =>
            inlineValuesToTex(attributes.targetT.inl)(ctx).mapc(str => s"\\part{${str}}")

          case BibQuery => inlineToTex(bibDB.convert(mcro))(ctx)
          case Cite =>
            val cmndCtx = attributes.named.get("style") match
              case Some("author") => ctx.ret("citet")
              case Some("inline") => ctx.ret("bibentry").useFeature("bibentry")
              case _              => ctx.ret("cite")

            nbrs(attributes)(cmndCtx).mapc(str => s"$str\\${cmndCtx.data}{${attributes.target}}")

          case Ref =>
            if attributes.named.contains("scope") then
              warn(s"scope support unclear", mcro)
              ()
            val candidates =
              References.filterCandidates(
                article.sourceDoc.path,
                articleDirectory.labels.getOrElse(attributes.target, Nil)
              )

            if candidates.sizeIs > 1 then
              scribe.error(
                s"multiple resolutions for ${attributes.target}" +
                article.sourceDoc.reporter(mcro) +
                s"\n\tresolutions are in: ${candidates.map(c => c.scope).mkString("\n\t", "\n\t", "\n\t")}"
              )

            candidates.headOption match
              case None =>
                scribe.error(s"no resolution found for ${attributes.target}" + article.sourceDoc.reporter(mcro))
                ctx.empty
              case Some(candidate) =>
                // TODO: existence of line is unchecked
                val label = References.getLabel(candidate).get + attributes.named.getOrElse("line", "")
                attributes.named.get("style") match
                  case Some("plain") =>
                    inlineValuesToTex(attributes.argumentsT.head.inl)(ctx).mapc { str =>
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
                if article.settings.get("footnotelinks").contains("disabled") then textref
                else s"$textref\\footnote{$plainurl}"
              else plainurl
            }.useFeature("href")

          case Lookup =>
            project.definitions.get(attributes.target).orElse(attributes.named.get("default").map(Text.of)) match
              case Some(res) =>
                inlineValuesToTex(res.inl)(ctx).map(Chain(_))
              case None =>
                warn(s"unknown name »${attributes.target}«", mcro)
                ctx.retc(latexencode(attributes.target))

          case Other("footnote") =>
            inlineValuesToTex(attributes.targetT.inl)(ctx).map(target => s"\\footnote{$target}").single

          case Other("tableofcontents") =>
            ctx.useFeature("tableofcontents").retc(
              List("\\cleardoublepage", "\\tableofcontents*", "\\mainmatter").mkString("\n")
            )

          case Other(_) =>
            val str = warn(s"unknown macro", mcro)
            ctx.retc(str)

          case Image =>
            val target = attributes.named.getOrElse(ImageTarget.Tex.name, attributes.target)
            article.sourceDoc.resolve(target) match
              case None =>
                ctx.retc(warn(s"could not find path", mcro))
              case Some(data) =>
                val mw = java.lang.Double.parseDouble(attributes.named.getOrElse("maxwidth", "1"))
                ctx.ret(Chain(s"\\includegraphics[max width=$mw\\columnwidth]{${data.absolute}}")).useFeature(
                  "graphics"
                )

          case Include =>
            val str: String = warn(s"tex backend does not allow inline includes", mcro)
            ctx.retc(str)
  def warn(msg: String, im: Directive): String =
    val macroStr = SastToScimConverter(bibDB).macroToScim(im)
    scribe.warn(s"$msg: ⸢$macroStr⸥${article.sourceDoc.reporter(im)}")
    macroStr

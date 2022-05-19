package scitzen.outputs

import better.files.File
import cats.data.Chain
import scitzen.contexts.ConversionContext
import scitzen.extern.ImageTarget
import scitzen.generic.{Article, DocumentDirectory, Project, References, Reporter, SastRef}
import scitzen.sast.DCommand.{Cite, Code, Comment, Def, Emph, Image, Include, Link, Lookup, Math, Other, Ref, Strong}
import scitzen.sast.*
import scitzen.outputs.SastToTexConverter.latexencode
import scitzen.sast.Attribute.{Plain, Positional}

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
    cwf: File,
    reporter: Reporter,
    includeResolver: DocumentDirectory,
    labels: Map[String, List[SastRef]],
):

  val cwd = cwf.parent

  type CtxCS  = ConversionContext[Chain[String]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[?]

  def articleHeader(article: Article, cta: Cta): CtxCS =
    val hasToc = cta.features.contains("tableofcontents")
    val fm     = if hasToc then Chain("\\frontmatter") else Chain.empty

    val ilc    = inlineValuesToTex(article.header.title.inl)(cta)
    val author = article.header.attributes.named.get("author").fold("")(n => s"\\author{${latexencode(n)}}")
    ilc.ret(fm :+ s"\\title{${ilc.data}}$author\\scitzenmaketitle")

  def convert(mainSast: List[Sast])(ctx: Cta): CtxCS =
    sastSeqToTex(mainSast)(ctx)

  def sastSeqToTex(b: Seq[Sast])(ctx: Cta): CtxCS =
    ctx.fold(b) { (ctx, sast) => sastToTex(sast)(ctx) }

  val sectioning: Int => String = nesting => {
    // "book", "part", "chapter",
    val secs = Array("chapter", "section", "subsection", "paragraph")
    val sec  = secs.lift(nesting).getOrElse("paragraph")
    sec
  }

  def sastToTex(sast: Sast)(ctx: Cta): CtxCS =
    sast match
      case tlBlock: Block => blockToTex(tlBlock)(ctx)

      case section @ Section(title, prefix, attr) =>
        val ilc = inlineValuesToTex(title.inl)(ctx)

        val pushed   = ilc.push(section)
        val numbered = if attr.named.get("style").contains("unnumbered") then "*" else ""
        val header = prefix match
          case "==" =>
            s"\\chapter$numbered[${ilc.data}]{${ilc.data}}"
          case _ =>
            val shift = 1 - pushed.sections.collectFirst { case Section(_, "==", _) => () }.size
            val sec   = sectioning(prefix.length - shift)
            s"\\$sec$numbered{${ilc.data}}"

        val label = attr.named.get("label").map(l => s"\\label{$l}").toList

        pushed.retc(header) :++ Chain.fromSeq(label)

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
            project.resolve(cwd, mcro.attributes.target).flatMap(includeResolver.byPath.get) match
              case Some(doc) =>
                val included = includeResolver.byPath(doc.file)

                new SastToTexConverter(project, doc.file, doc.reporter, includeResolver, labels)
                  .sastSeqToTex(included.sast)(ctx)

              case None =>
                scribe.error(s"unknown include ${mcro.attributes.target}" + reporter(mcro))
                ctx.empty

          case other =>
            inlineValuesToTex(List(mcro))(ctx).single

  def texbox(name: String, attributes: Attributes, content: Seq[Sast])(ctx: Cta): CtxCS =
    val args      = attributes.legacyPositional.tail
    val optionals = if args.isEmpty then "" else args.mkString("[", "; ", "]")
    val label     = attributes.named.get("label").map(s => s"\\label{$s}").getOrElse("")
    s"\\begin{$name}$optionals$label" +:
      sastSeqToTex(content)(ctx) :+
      s"\\end{$name}"

  def blockToTex(tlblock: Block)(ctx: Cta): CtxCS =
    val innerCtx: CtxCS =
      tlblock.content match
        case Paragraph(content) => inlineValuesToTex(content.inl)(ctx).single :+ "" :+ ""

        case Parsed(_, blockContent) =>
          tlblock.command match
            case "figure" =>
              val (figContent, caption) =
                blockContent.lastOption match
                  case Some(Block(_, Paragraph(content), _)) =>
                    val captionstr = inlineValuesToTex(content.inl)(ctx)
                    (blockContent.init, captionstr.map(str => s"\\caption{$str}"))
                  case _ =>
                    scribe.warn(s"figure has no caption" + reporter(tlblock.prov))
                    (blockContent, ctx.ret(""))
              "\\begin{figure}" +:
                "\\centerfloat" +:
                sastSeqToTex(figContent)(caption) :++
                Chain(
                  caption.data,
                  tlblock.attributes.named.get("label").fold("")(l => s"\\label{$l}"),
                  "\\end{figure}"
                )

            case name @ ("theorem" | "definition" | "proofbox" | "proof" | "lemma" | "example") =>
              texbox(name, tlblock.attributes, blockContent)(ctx).useFeature("framed")

            case name @ "abstract" => texbox(name, tlblock.attributes, blockContent)(ctx)

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

              case Some("text") =>
                val latexenc = latexencode(text).trim
                  .replace("\n", "\\newline{}\n")
                // appending the empty string adds another newline in the source code to separate the paragraph from the following text – the latexenc text does not have any newlines at the end because of the .trim
                ctx.ret(Chain("\\noindent", latexenc, ""))

              case _ =>
                val labeltext = tlblock.attributes.named.get("label") match
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
          case Code           => ctx.retc(s"\\texttt{${latexencode(attributes.target)}}")
          case Comment        => ctx.retc("")
          case Def            => ctx.retc("")
          case Emph           => inlineValuesToTex(attributes.targetT.inl)(ctx).mapc(str => s"\\emph{$str}")
          case Math           => ctx.retc(s"$$${attributes.target}$$")
          case Other("break") => ctx.retc(s"\\clearpage{}")
          case Other("rule") => inlineToTex(Directive(
              Ref,
              attributes.copy(raw =
                Seq(
                  Positional(Text(Seq(Directive(Other("smallcaps"), attributes)(mcro.prov))), None),
                  Plain("style", "plain"),
                  Positional(s"rule-${attributes.target}")
                )
              )
            )(
              mcro.prov
            ))(ctx)
          case Other("smallcaps") => ctx.retc(s"\\textsc{${attributes.target}}")
          case Other("raw")       => ctx.retc(attributes.named.getOrElse("tex", ""))
          case Other("todo") =>
            inlineValuesToTex(attributes.targetT.inl)(ctx).mapc(str => s"{\\color{red}TODO:${str}}")
          case Strong => inlineValuesToTex(attributes.targetT.inl)(ctx).mapc(str => s"\\textbf{$str}")
          case Other("partition") =>
            inlineValuesToTex(attributes.targetT.inl)(ctx).mapc(str => s"\\part{${str}}")

          case Cite =>
            val cmndCtx = attributes.named.get("style") match
              case Some("name")   => ctx.ret("citet")
              case Some("inline") => ctx.ret("bibentry").useFeature("bibentry")
              case _              => ctx.ret("cite")

            nbrs(attributes)(cmndCtx).mapc(str => s"$str\\${cmndCtx.data}{${attributes.target}}")

          case Ref =>
            val scope      = attributes.named.get("scope").flatMap(project.resolve(cwd, _)).getOrElse(cwf)
            val candidates = References.filterCandidates(scope, labels.getOrElse(attributes.target, Nil))

            if candidates.sizeIs > 1 then
              scribe.error(
                s"multiple resolutions for ${attributes.target}" +
                  reporter(mcro) +
                  s"\n\tresolutions are in: ${candidates.map(c => project.relativizeToProject(c.scope)).mkString("\n\t", "\n\t", "\n\t")}"
              )

            candidates.headOption match
              case None =>
                scribe.error(s"no resolution found for ${attributes.target}" + reporter(mcro))
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
              val target = attributes.target
              if attributes.legacyPositional.size > 1 then
                val name = "{" + latexencode(attributes.legacyPositional.head) + "}"
                s"\\href{$target}{$name}"
              else s"\\url{$target}"
            }.useFeature("href")

          case Lookup =>
            project.definitions.get(attributes.target) match
              case Some(res) =>
                inlineValuesToTex(res.inl)(ctx).map(Chain(_))
              case None =>
                scribe.warn(s"unknown name ${attributes.target}" + reporter(mcro))
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
            project.resolve(cwd, target) match
              case None =>
                ctx.retc(warn(s"could not find path", mcro))
              case Some(data) =>
                val mw = java.lang.Double.parseDouble(attributes.named.getOrElse("maxwidth", "1"))
                ctx.ret(Chain(s"\\includegraphics[max width=$mw\\columnwidth]{$data}")).useFeature(
                  "graphics"
                )

          case Include =>
            val str: String = warn(s"tex backend does not allow inline includes", mcro)
            ctx.retc(str)
  def warn(msg: String, im: Directive): String =
    val macroStr = SastToScimConverter.macroToScim(im)
    scribe.warn(s"$msg: ⸢$macroStr⸥${reporter(im)}")
    macroStr

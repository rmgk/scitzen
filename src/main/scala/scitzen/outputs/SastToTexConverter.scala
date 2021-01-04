package scitzen.outputs

import better.files.File
import cats.data.Chain
import scitzen.generic.{Article, ConversionContext, DocumentDirectory, Project, References, Reporter}
import scitzen.sast.MacroCommand.{
  Cite, Code, Comment, Def, Emph, Image, Include, Link, Lookup, Math, Other, Ref, Strong
}
import scitzen.sast._

class SastToTexConverter(project: Project, cwf: File, reporter: Reporter, includeResolver: DocumentDirectory) {

  val cwd = cwf.parent

  type CtxCS  = ConversionContext[Chain[String]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[_]

  def articleHeader(article: Article, cta: Cta): CtxCS = {
    val hasToc = cta.partialMacros.exists(_.command == MacroCommand.Other("tableofcontents"))
    val fm     = if (hasToc) Chain("\\frontmatter") else Chain.empty

    val ilc    = inlineValuesToTex(article.header.title.inl)(cta)
    val author = article.header.attributes.named.get("author").fold("")(n => s"\\author{${latexencode(n)}}")
    ilc.ret(fm :+ s"\\title{${ilc.data}}$author\\maketitle{}")

  }

  def convert(mainSast: List[Sast])(implicit ctx: Cta): CtxCS = {
    sastSeqToTex(mainSast)(ctx)
  }

  def latexencode(input: String): String = {
    val dummyForBSreplace = "»§ dummy to replace later ℓ«"
    val nobs              = input.replace("\\", dummyForBSreplace)
    val nosimple = "&%$#_{}".toList.map(_.toString).foldLeft(nobs) { (acc, char) =>
      acc.replace(char, s"\\$char")
    }
    val nomuch = List(
      "~" -> "\\textasciitilde{}",
      "^" -> "\\textasciicircum{}",
      "`" -> "\\textasciigrave{}"
    )
      .foldLeft(nosimple) {
        case (acc, (char, rep)) =>
          acc.replace(char, rep)
      }
    nomuch.replace(dummyForBSreplace, "\\textbackslash{}")
  }

  def sastSeqToTex(b: Seq[Sast])(implicit ctx: Cta): CtxCS = {
    ctx.fold(b) { (ctx, sast) => sastToTex(sast)(ctx) }
  }

  val sectioning: Int => String = nesting => {
    // "book", "part", "chapter",
    val secs = Array("chapter", "section", "subsection", "paragraph")
    val sec  = secs.lift(nesting).getOrElse("paragraph")
    sec
  }

  def sastToTex(sast: Sast)(implicit ctx: Cta): CtxCS =
    sast match {
      case tlBlock: Block => blockToTex(tlBlock)

      case section @ Section(title, prefix, attr) =>
        val ilc = inlineValuesToTex(title.inl)(ctx)

        val pushed = ilc.push(section)
        val header = prefix match {
          case "==" =>
            s"\\chapter{${ilc.data}}"
          case _ =>
            val shift = 1 - pushed.sections.collectFirst { case Section(_, "==", _) => () }.size
            val sec   = sectioning(prefix.length - shift)
            s"\\$sec{${ilc.data}}"
        }

        val label = attr.named.get("label").map(l => s"\\label{$l}").toList

        pushed.retc(header) :++ Chain.fromSeq(label)

      case Slist(children) =>
        children match {
          case Nil => ctx.ret(Chain.nil)

          case ListItem(_, _, None | Some(Slist(_))) :: _ =>
            "\\begin{itemize}" +:
              ctx.fold[ListItem, String](children) { (ctx, child) =>
                val inlineCtx  = inlineValuesToTex(child.text.inl)(ctx).map(s => Chain(s"\\item{$s}"))
                val contentCtx = child.content.fold(inlineCtx.empty[String])(sastToTex(_)(inlineCtx))
                inlineCtx.data ++: contentCtx
              } :+
              "\\end{itemize}"

          case ListItem(_, _, _) :: _ =>
            ctx.fold[ListItem, String](children) { (ctx, child) =>
              val inlinesCtx = inlineValuesToTex(child.text.inl)(ctx).map(s => s"\\item[$s]{}")
              inlinesCtx.data +: child.content.fold(inlinesCtx.empty[String])(sastToTex(_)(inlinesCtx))
            }.map { content =>
              "\\begin{description}" +: content :+ "\\end{description}"
            }

        }

      case mcro @ Macro(_, _) => mcro match {
          case Macro(Image, attributes) =>
            val target = attributes.target

            project.resolve(cwd, target) match {
              case None =>
                ctx.retc(warn(s"could not find path", mcro))
              case Some(data) =>
                ctx.ret(Chain(s"\\noindent{}\\includegraphics[max width=\\columnwidth]{$data}\n")).useFeature(
                  "graphics"
                )
            }

          case Macro(Include, attributes) =>
            project.resolve(cwd, attributes.target).flatMap(includeResolver.byPath.get) match {
              case Some(doc) =>
                val included = includeResolver.byPath(doc.file)

                new SastToTexConverter(project, doc.file, doc.reporter, includeResolver)
                  .sastSeqToTex(included.sast)(ctx)

              case None =>
                scribe.error(s"unknown include ${attributes.target}" + reporter(attributes.prov))
                ctx.empty
            }

          case other =>
            inlineValuesToTex(List(other)).single
        }
    }

  def texbox(name: String, attributes: Attributes, content: Seq[Sast])(implicit ctx: Cta): CtxCS = {
    val args      = attributes.positional.tail
    val optionals = if (args.isEmpty) "" else args.mkString("[", "; ", "]")
    val label     = attributes.named.get("label").map(s => s"\\label{$s}").getOrElse("")
    s"\\begin{$name}$optionals$label" +:
      sastSeqToTex(content) :+
      s"\\end{$name}"
  }

  def blockToTex(tlblock: Block)(implicit ctx: Cta): CtxCS = {
    val innerCtx: CtxCS = tlblock.content match {
      case Paragraph(content) => inlineValuesToTex(content.inl).single :+ "" :+ ""

      case Parsed(_, blockContent) =>
        tlblock.attributes.positional.headOption match {
          case Some(blockname) =>
            blockname match {
              case "figure" =>
                val (figContent, caption) = {
                  blockContent.lastOption match {
                    case Some(Block(_, Paragraph(content))) =>
                      val captionstr = inlineValuesToTex(content.inl).data
                      (blockContent.init, s"\\caption{$captionstr}")
                    case _ =>
                      scribe.warn(s"figure has no caption" + reporter(tlblock.attributes.prov))
                      (blockContent, "")
                  }
                }
                "\\begin{figure}" +:
                  "\\centerfloat" +:
                  sastSeqToTex(figContent) :++
                  Chain(
                    caption,
                    tlblock.attributes.named.get("label").fold("")(l => s"\\label{$l}"),
                    "\\end{figure}"
                  )

              case name @ ("theorem" | "definition" | "proofbox" | "proof" | "lemma" | "example") =>
                texbox(name, tlblock.attributes, blockContent).useFeature("framed")

              case name @ "abstract" => texbox(name, tlblock.attributes, blockContent)

              case _ =>
                sastSeqToTex(blockContent)
            }

          case None =>
            sastSeqToTex(blockContent)
        }

      case Fenced(text) =>
        tlblock.attributes.positional.headOption match {

          case Some("text") =>
            val latexenc = latexencode(text).trim
              .replaceAll("\n{2,}", """\\newline{}\\noindent{}""")
              .replace("\n", "\\newline{}\n")
            ctx.ret(Chain("\\noindent", latexenc))

          case _ =>
            val labeltext = tlblock.attributes.named.get("label") match {
              case None => text
              case Some(label) =>
                text.replaceAll(""":§([^§]*?)§""", s"""(*@\\\\label{$label$$1}@*)""")
            }
            val restext =
              if (!tlblock.attributes.positional.contains("highlight")) labeltext
              else {
                labeltext.replaceAll(""":hl§([^§]*?)§""", s"""(*@\\\\textbf{$$1}@*)""")

              }
            ctx.ret(Chain(s"\\begin{lstlisting}", restext, "\\end{lstlisting}")).useFeature("listings")

        }

      case SpaceComment(_) => ctx.empty

    }

    if (project.config.notes.contains("hide")) innerCtx
    else {
      tlblock.attributes.namedT.get("note").fold(innerCtx) { note =>
        inlineValuesToTex(note.inl)(innerCtx).map { (content: String) =>
          s"\\sidepar{$content}%" +: innerCtx.data
        }.useFeature("sidepar")
      }
    }
  }

  def nbrs(attributes: Attributes)(implicit ctx: Cta): Ctx[String] = {
    attributes.argumentsT match {
      case Nil => ctx.ret("")
      case arg :: _ =>
        inlineValuesToTex(attributes.argumentsT.head.inl).map { str =>
          s"${str}~"
        }
    }
  }

  def inlineValuesToTex(inners: Seq[Inline])(implicit ctx: Cta): Ctx[String] =
    ctx.fold(inners) { (ctx: Ctx[Chain[String]], inline) => inlineToTex(inline)(ctx) }.map(_.toList.mkString(""))

  def inlineToTex(inln: Inline)(implicit ctx: Cta): CtxCS =
    inln match {
      case InlineText(str)            => ctx.retc(latexencode(str))
      case Macro(Code, attrs)         => ctx.retc(s"\\texttt{${latexencode(attrs.target)}}")
      case Macro(Comment, _)          => ctx.retc("")
      case Macro(Def, _)              => ctx.retc("")
      case Macro(Emph, attrs)         => inlineValuesToTex(attrs.targetT.inl).mapc(str => s"\\emph{$str}")
      case Macro(Math, attrs)         => ctx.retc(s"$$${attrs.target}$$")
      case Macro(Other("break"), _)   => ctx.retc(s"\\clearpage{}")
      case Macro(Other("rule"), attr) => ctx.retc(s"\\textsc{${attr.target}}")
      case Macro(Other("raw"), attr)  => ctx.retc(attr.named.getOrElse("tex", ""))
      case Macro(Other("todo"), attr) =>
        inlineValuesToTex(attr.targetT.inl).mapc(str => s"{\\color{red}TODO:${str}}")
      case Macro(Strong, attrs) => inlineValuesToTex(attrs.targetT.inl).mapc(str => s"\\textbf{$str}")
      case Macro(Other("partition"), attrs) =>
        inlineValuesToTex(attrs.targetT.inl).mapc(str => s"\\part{${str}}")

      case Macro(Cite, attr) =>
        val cmndCtx = attr.named.get("style") match {
          case Some("name")   => ctx.ret("citet")
          case Some("inline") => ctx.ret("bibentry").useFeature("bibentry")
          case _              => ctx.ret("cite")
        }

        nbrs(attr)(cmndCtx).mapc(str => s"$str\\${cmndCtx.data}{${attr.target}}")

      case Macro(Ref, attr) =>
        val scope      = attr.named.get("scope").flatMap(project.resolve(cwd, _)).getOrElse(cwf)
        val candidates = References.filterCandidates(scope, ctx.resolveRef(attr.target))

        if (candidates.sizeIs > 1)
          scribe.error(
            s"multiple resolutions for ${attr.target}" +
              reporter(attr.prov) +
              s"\n\tresolutions are in: ${candidates.map(c => project.relativizeToProject(c.scope)).mkString("\n\t", "\n\t", "\n\t")}"
          )

        candidates.headOption match {
          case None =>
            scribe.error(s"no resolution found for ${attr.target}" + reporter(attr.prov))
            ctx.empty
          case Some(candidate) =>
            val label = References.getLabel(candidate).get + attr.named.getOrElse("line", "")
            nbrs(attr).mapc { str => s"${str}\\ref{${label}}" }
        }

      case Macro(Link, attributes) =>
        ctx.retc {
          val target = attributes.target
          if (attributes.positional.size > 1) {
            val name = "{" + latexencode(attributes.positional.head) + "}"
            s"\\href{$target}{$name}"
          } else s"\\url{$target}"
        }.useFeature("href")

      case Macro(Lookup, attributes) =>
        project.definitions.get(attributes.target) match {
          case Some(res) =>
            inlineValuesToTex(res.inl)(ctx).map(Chain(_))
          case None =>
            scribe.warn(s"unknown name ${attributes.target}" + reporter(attributes.prov))
            ctx.retc(latexencode(attributes.target))
        }

      case Macro(Other("footnote"), attributes) =>
        inlineValuesToTex(attributes.targetT.inl).map(target => s"\\footnote{$target}").single

      case toc @ Macro(Other("tableofcontents"), _) =>
        ctx.addMacro(toc).retc(
          List("\\clearpage", "\\tableofcontents*", "\\clearpage", "\\mainmatter").mkString("\n")
        )

      case im @ Macro(Other(_), _) =>
        val str = warn(s"unknown macro", im)
        ctx.retc(str)

      case im @ Macro(Image | Include, _) =>
        val str: String = warn(s"tex backend does not allow inline images or includes", im)
        ctx.retc(str)
    }
  def warn(msg: String, im: Macro): String = {
    val macroStr = SastToScimConverter.macroToScim(im)
    scribe.warn(s"$msg: ⸢$macroStr⸥${reporter(im)}")
    macroStr
  }
}

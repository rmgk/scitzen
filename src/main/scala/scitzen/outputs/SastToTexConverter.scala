package scitzen.outputs

import better.files.File
import cats.data.Chain
import scitzen.generic.{Article, ConversionContext, DocumentDirectory, Project, Reporter}
import scitzen.parser.MacroCommand.{
  Cite, Code, Comment, Def, Emph, Image, Include, Label, Link, Lookup, Math, Other, Ref, Strong
}
import scitzen.parser.Sast._
import scitzen.parser.{Attributes, Inline, InlineText, MacroCommand, Sast}

class SastToTexConverter(project: Project, cwd: File, reporter: Reporter, includeResolver: DocumentDirectory) {

  type CtxCS  = ConversionContext[Chain[String]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[_]

  def articleHeader(article: Article, cta: Cta): CtxCS = {
    val hasToc = cta.partialMacros.exists(_.command == MacroCommand.Other("tableofcontents"))
    val fm     = if (hasToc) Chain("\\frontmatter") else Chain.empty

    val ilc = inlineValuesToTex(article.header.title.inl)(cta)
    ilc.ret(fm :+ s"\\title{${ilc.data}}\\maketitle{}")

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
          case other =>
            val shift = 1 - pushed.stack.collectFirst { case Section(_, "==", _) => () }.size
            val sec   = sectioning(prefix.length - shift)
            s"\\$sec{${ilc.data}}"
        }

        pushed.retc(header)

      case Slist(children) =>
        children match {
          case Nil => ctx.ret(Chain.nil)

          case ListItem(m, _, None | Some(Slist(_))) :: _ =>
            "\\begin{itemize}" +:
              ctx.fold[ListItem, String](children) { (ctx, child) =>
                val inlineCtx  = inlineValuesToTex(child.text.inl)(ctx).map(s => Chain(s"\\item{$s}"))
                val contentCtx = child.content.fold(inlineCtx.empty[String])(sastToTex(_)(inlineCtx))
                inlineCtx.data ++: contentCtx
              } :+
              "\\end{itemize}"

          case ListItem(m, _, _) :: _ =>
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
                scribe.error(s"Not relative path: $mcro")
                ctx.empty
              case Some(data) =>
                ctx.ret(Chain(s"\\noindent{}\\includegraphics[max width=\\columnwidth]{$data}\n"))
            }

          case Macro(Include, attributes) =>
            project.resolve(cwd, attributes.target).flatMap(includeResolver.byPath.get) match {
              case Some(doc) =>
                val included = includeResolver.byPath(doc.file)
                val stack    = ctx.stack

                new SastToTexConverter(project, doc.file.parent, doc.reporter, includeResolver)
                  .sastSeqToTex(included.sast)(ctx.push(sast)).copy(stack = stack)

              case None =>
                scribe.error(s"unknown include ${attributes.target}" + reporter(attributes.prov))
                ctx.empty
            }

          case other =>
            inlineValuesToTex(List(other)).single
        }
    }

  def texbox(name: String, args: Seq[String], content: Seq[Sast])(implicit ctx: Cta): CtxCS = {
    val optionals = if (args.isEmpty) "" else args.mkString("[", "; ", "]")
    s"\\begin{$name}$optionals" +:
      sastSeqToTex(content) :+
      s"\\end{$name}"
  }

  def blockToTex(tlblock: Block)(implicit ctx: Cta): CtxCS =
    tlblock.content match {
      case Paragraph(content) => inlineValuesToTex(content.inl).single :+ "" :+ ""

      case Parsed(delimiter, blockContent) =>
        tlblock.attributes.positional.headOption match {
          case Some(blockname) =>
            blockname match {
              case "figure" =>
                val (figContent, caption) = {
                  blockContent.lastOption match {
                    case Some(inner @ Block(_, Paragraph(content))) =>
                      val captionstr = inlineValuesToTex(content.inl).data
                      (blockContent.init, s"\\caption{$captionstr}")
                    case other =>
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

              case name @ ("theorem" | "definition" | "proofbox" | "proof" | "lemma" | "example" | "abstract") =>
                texbox(name, tlblock.attributes.positional.tail, blockContent)

              case other =>
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

          case other =>
            val restext = tlblock.attributes.named.get("label") match {
              case None => text
              case Some(label) =>
                text.replaceAll(""":§([^§]*?)§""", s"""(*@\\\\label{$label$$1}@*)""")
            }
            ctx.ret(Chain(s"\\begin{lstlisting}", restext, "\\end{lstlisting}"))

        }

      case SpaceComment(_) => ctx.empty

    }

  def nbrs(attributes: Attributes): String = {
    if (attributes.arguments.nonEmpty) s"${attributes.arguments.head}~"
    else ""
  }

  def inlineValuesToTex(inners: Seq[Inline])(implicit ctx: Cta): Ctx[String] =
    ctx.fold(inners) { (ctx: Ctx[Chain[String]], inline) => inlineToTex(inline)(ctx) }.map(_.toList.mkString(""))

  def inlineToTex(inline: Inline)(implicit ctx: Cta): CtxCS =
    inline match {
      case InlineText(str)                    => ctx.retc(latexencode(str))
      case Macro(Cite, attr)                  => ctx.retc(s"${nbrs(attr)}\\cite{${attr.target}}")
      case Macro(Code, attrs)                 => ctx.retc(s"\\texttt{${latexencode(attrs.target)}}")
      case Macro(Comment, attr)               => ctx.retc("")
      case Macro(Def, _)                      => ctx.retc("")
      case Macro(Emph, attrs)                 => ctx.retc(s"\\emph{${latexencode(attrs.target)}}")
      case Macro(Label, attr)                 => ctx.retc(s"\\label{${attr.target}}")
      case Macro(Math, attrs)                 => ctx.retc(s"$$${attrs.target}$$")
      case Macro(Other("break"), attrs)       => ctx.retc(s"\\clearpage{}")
      case Macro(Other("subparagraph"), attr) => ctx.retc(s"\\subparagraph{${attr.target}}")
      case Macro(Other("textsc"), attr)       => ctx.retc(s"\\textsc{${attr.target}}")
      case Macro(Other("todo"), attr)         => ctx.retc(s"{\\color{red}TODO:${attr.target}}")
      case Macro(Ref, attr)                   => ctx.retc(s"${nbrs(attr)}\\ref{${attr.target}}")
      case Macro(Strong, attrs)               => ctx.retc(s"\\textbf{${latexencode(attrs.target)}}")

      case Macro(Link, attributes) =>
        ctx.retc {
          val target = attributes.target
          if (attributes.positional.size > 1) {
            val name = "{" + attributes.positional.head + "}"
            s"\\href{$target}{$name}"
          } else s"\\url{$target}"
        }

      case Macro(Lookup, attributes) =>
        ctx.retc {
          project.config.definitions.getOrElse(
            attributes.target, {
              scribe.warn(s"unknown name ${attributes.target}" + reporter(attributes.prov))
              attributes.target
            }
          )
        }

      case Macro(Other("footnote"), attributes) =>
        inlineValuesToTex(attributes.targetT.inl).map(target =>s"\\footnote{$target}").single

      case toc @ Macro(Other("tableofcontents"), attributes) =>
        ctx.addMacro(toc).retc(
          List("\\clearpage", "\\tableofcontents*", "\\clearpage", "\\mainmatter").mkString("\n")
        )

      case im @ Macro(Other(command), attributes) =>
        val str = SastToScimConverter.macroToScim(im)
        scribe.warn(s"unknown macro “$str”" + reporter(im))
        ctx.retc(str)

      case im @ Macro(Image | Include, attributes) =>
        val str = SastToScimConverter.macroToScim(im)
        scribe.warn(s"tex backend does not allow inline images or includes" + reporter(im))
        ctx.retc(str)
    }
}

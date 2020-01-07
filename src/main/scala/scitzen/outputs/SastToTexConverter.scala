package scitzen.outputs

import better.files.File
import cats.data.Chain
import scitzen.generic.Sast._
import scitzen.generic.{ConversionContext, ParsedDocument, Project, Reporter, Sast, Scope}
import scitzen.parser.MacroCommand.{Cite, Comment, Def, Image, Include, Label, Link, Other, Quote, Ref}
import scitzen.parser.{Inline, InlineText, Macro}


class SastToTexConverter(project: Project,
                         cwd: File,
                         reporter: Reporter,
                         preproc: ParsedDocument => SastToSastConverter
                        ) {

  type CtxCS = ConversionContext[Chain[String]]
  type Ctx[T] = ConversionContext[T]
  type Cta = Ctx[_]

  def convert(mainSast: List[Sast])(implicit ctx: Cta): CtxCS = mainSast match {
    case List(Section(title, content, _)) =>
      val ilc = inlineValuesToTex(title.inline)
      s"\\title{${ilc.data}}\\maketitle{}" +:
      ilc.withScope(new Scope(2))(sastSeqToTex(content)(_))

    case list => ctx.withScope(new Scope(1))(sastSeqToTex(list)(_))
  }

  def latexencode(input: String): String = {
    val dummyForBSreplace = "»§ dummy to replace later ℓ«"
    val nobs              = input.replaceAllLiterally("\\", dummyForBSreplace)
    val nosimple          = "&%$#_{}".toList.map(_.toString).foldLeft(nobs) { (acc, char) =>
      acc.replaceAllLiterally(char, s"\\$char")
    }
    val nomuch            = List(
      "~" -> "\\textasciitilde{}",
      "^" -> "\\textasciicircum{}",
      "`" -> "\\textasciigrave{}")
    .foldLeft(nosimple) { case (acc, (char, rep)) =>
      acc.replaceAllLiterally(char, rep)
    }
    nomuch.replaceAllLiterally(dummyForBSreplace, "\\textbackslash{}")
  }

  def sastSeqToTex(b: Seq[Sast])(implicit ctx: Cta): CtxCS = {
    ctx.fold(b) { (ctx, sast) => sastToTex(sast)(ctx) }
  }

  def sastToTex(sast: Sast)(implicit ctx: Cta): CtxCS = sast match {

    case tlBlock: SBlock => blockToTex(tlBlock)

    case Section(title, contents, _) =>
      val sec = sectioning(ctx.scope)
      val ilc = inlineValuesToTex(title.inline)
      s"\\$sec{${ilc.data}}" +:
      ilc.incScope(sastSeqToTex(contents)(_))

    case Slist(children) =>
      children match {
        case Nil => ctx.ret(Chain.nil)

        case SlistItem(m, _) :: _ if m.contains(":") =>
          ctx.fold[SlistItem, String](children) { (ctx, child) =>
            s"\\item[${child.marker.replaceAllLiterally(":", "")}]" +:
            sastSeqToTex(child.content)(ctx)
          }.map { content =>
            "\\begin{description}" +: content :+ "\\end{description}"
          }

        case other =>
          "\\begin{itemize}" +:
          ctx.fold[SlistItem, String](children) { (ctx, child) =>
            s"\\item" +: sastSeqToTex(child.content)(ctx)
          } :+
          "\\end{itemize}"
      }

    case SMacro(mcro) => mcro match {
      case Macro(Image, attributes) =>
        val target = attributes.target

        project.resolve(cwd, target) match {
          case None       =>
            scribe.error(s"Not relative path: $mcro")
            ctx.empty
          case Some(data) =>
            ctx.ret(Chain(s"\\noindent{}\\includegraphics[width=\\columnwidth]{$data}\n"))
        }


      case Macro(Include, attributes) =>
        project.findDoc(cwd, attributes.target) match {
          case Some(doc) =>
            ctx.withScope(new Scope(3)){ ctx =>
              val preprocctx = preproc(doc.parsed).convertSeq(doc.sast)(ctx)
              new SastToTexConverter(project, doc.parsed.file.parent, doc.parsed.reporter, preproc)
              .sastSeqToTex(preprocctx.data.toList)(preprocctx)}

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

  def blockToTex(tlblock: SBlock)(implicit ctx: Cta): CtxCS = tlblock.content match {
    case Paragraph(content) => inlineValuesToTex(content.inline).single :+ ""

    case Parsed(delimiter, blockContent) =>
      delimiter.charAt(0) match {
        case '=' =>
          tlblock.attributes.positional.headOption match {
            case Some(blockname) =>
              blockname match {
                case "figure" =>
                  val (figContent, caption) = {
                    blockContent.lastOption match {
                      case Some(inner @ SBlock(_, Paragraph(content))) =>
                        val captionstr = inlineValuesToTex(content.inline).single.data.iterator.mkString("\n")
                        (blockContent.init,
                        s"\\caption{$captionstr}")
                      case other                                       =>
                        scribe.warn(s"figure has no caption")
                        (blockContent, "")
                    }
                  }
                  "\\begin{figure}" +:
                  sastSeqToTex(figContent) :++
                  Chain(
                    caption,
                    tlblock.attributes.named.get("label").fold("")(l => s"\\label{$l}"),
                    "\\end{figure}"
                    )

                case name @ ("theorem" | "definition" | "proofbox" | "proof" | "lemma" | "example") =>

                  texbox(name, tlblock.attributes.positional.tail, blockContent)


                case other =>
                  sastSeqToTex(blockContent)
              }

            case None =>
              sastSeqToTex(blockContent)
          }

        // space indented blocks are currently only used for description lists
        // they are parsed and inserted as if the indentation was not present
        case ' ' => sastSeqToTex(blockContent)
        case _   => sastSeqToTex(blockContent)
      }

    case Fenced(text) =>
      tlblock.attributes.positional.headOption match {

        case Some("text") =>
          val latexenc = latexencode(text).trim
                                          .replaceAll("\n{2,}", """\\newline{}\\noindent{}""")
                                          .replaceAllLiterally("\n", "\\newline{}\n")
          ctx.ret(Chain("\\noindent", latexenc))

        case other  =>
          val restext = tlblock.attributes.named.get("label") match {
            case None        => text
            case Some(label) =>
              text.replaceAll(""":§([^§]*?)§""", s"""(*@\\\\label{$label$$1}@*)""")
          }
          ctx.ret(Chain(s"\\begin{lstlisting}", restext, "\\end{lstlisting}"))

      }

    case SpaceComment(_) => ctx.empty


  }

  val sectioning: Scope => String = nesting => {
    val secs = Array("book", "part", "chapter", "section", "subsection", "paragraph")
    val sec  = secs.lift(nesting.level).getOrElse("paragraph")
    sec
  }
  def inlineValuesToTex(inners: Seq[Inline])(implicit ctx: Cta): Ctx[String] = ctx.ret(inners.map {
    case InlineText(str) => latexencode(str)

    case Macro(Def, _) => ""

    case Macro(Quote(q), inner2)    =>
      val inner = latexencode(inner2.target)
      //scribe.warn(s"inline quote $q: $inner; ${post.sourcePath}")
      q.head match {
        case '_' => s"\\emph{$inner}"
        case '*' => s"\\textbf{$inner}"
        case '`' => s"\\texttt{$inner}"
        case '$' => s"$$${inner2.target}$$"
      }
    case Macro(Comment, attributes) => ""

    case Macro(Ref, attributes) => s"\\ref{${attributes.target}}"

    case Macro(Cite, attributes) =>
      s"\\cite{${attributes.target}}"

    case Macro(Link, attributes) =>
      val target = attributes.target
      if (attributes.positional.size > 1) {
        val name = """{"""" + attributes.positional.head + """"}"""
        s"\\href{$target}{$name}"
      }
      else s"\\url{$target}"

    case Macro(Other("n"), attributes) if project.documentManager.attributes.contains(attributes.target) =>
      project.documentManager.attributes(attributes.target)

    case Macro(Other("footnote"), attributes) =>
      val target = latexencode(attributes.target)
      s"\\footnote{$target}"

    case Macro(Label, attributes) => s"\\label{${attributes.target}}"

    case Macro(Other("tableofcontents"), attributes) =>
      List("\\clearpage", "\\tableofcontents*", "\\clearpage").mkString("\n")

    case Macro(Other("subparagraph"), attributes) =>
      s"\\subparagraph{${attributes.target}}"

    case Macro(Other("textsc"), attributes) =>
      s"\\textsc{${attributes.target}}"

    case im @ Macro(command, attributes) =>
      scribe.warn(s"inline macro “$command[$attributes]”")
      s"$command[${attributes.all.mkString(",")}]"
  }.mkString(""))

  def reportPos(m: Macro): String = reporter(m)

}

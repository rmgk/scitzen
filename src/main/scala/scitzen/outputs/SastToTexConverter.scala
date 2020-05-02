package scitzen.outputs

import better.files.File
import cats.data.Chain
import scitzen.generic.Sast._
import scitzen.generic.{ConversionContext, Project, Reporter, Sast}
import scitzen.parser.MacroCommand.{Cite, Code, Comment, Def, Emph, Image, Include, Label, Link, Math, Other, Ref, Strong}
import scitzen.parser.{Inline, InlineText, Macro}


class SastToTexConverter(project: Project,
                         cwd: File,
                         reporter: Reporter,
                         includeResolver: Map[File, Seq[Sast]]
                        ) {

  type CtxCS = ConversionContext[Chain[String]]
  type Ctx[T] = ConversionContext[T]
  type Cta = Ctx[_]

  def convert(mainSast: List[Sast])(implicit ctx: Cta): CtxCS = mainSast match {
    case List(section @ Section(title, content, _)) =>
      val ilc = inlineValuesToTex(title.inline)
      ilc.push(section)
         .retc(s"\\title{${ilc.data}}\\maketitle{}")

    case list => sastSeqToTex(list)(ctx)
  }

  def latexencode(input: String): String = {
    val dummyForBSreplace = "»§ dummy to replace later ℓ«"
    val nobs              = input.replace("\\", dummyForBSreplace)
    val nosimple          = "&%$#_{}".toList.map(_.toString).foldLeft(nobs) { (acc, char) =>
      acc.replace(char, s"\\$char")
    }
    val nomuch            = List(
      "~" -> "\\textasciitilde{}",
      "^" -> "\\textasciicircum{}",
      "`" -> "\\textasciigrave{}")
    .foldLeft(nosimple) { case (acc, (char, rep)) =>
      acc.replace(char, rep)
    }
    nomuch.replace(dummyForBSreplace, "\\textbackslash{}")
  }

  def sastSeqToTex(b: Seq[Sast])(implicit ctx: Cta): CtxCS = {
    ctx.fold(b) { (ctx, sast) => sastToTex(sast)(ctx) }
  }

  def sastToTex(sast: Sast)(implicit ctx: Cta): CtxCS = sast match {

    case NoContent => ctx.empty

    case tlBlock: SBlock => blockToTex(tlBlock)

    case section @ Section(title, level, _) =>
      val sec = sectioning(level)
      val ilc = inlineValuesToTex(title.inline)
      ilc.push(section).retc(s"\\$sec{${ilc.data}}")

    case Slist(children) =>
      children match {
        case Nil => ctx.ret(Chain.nil)

        case SlistItem(m, text, NoContent) :: _ =>
          "\\begin{itemize}" +:
          ctx.fold[SlistItem, String](children) { (ctx, child) =>
            inlineValuesToTex(child.text.inline)(ctx).map(s => Chain(s"\\item" + s))
          } :+
          "\\end{itemize}"

        case SlistItem(m, _, _) :: _ =>
          ctx.fold[SlistItem, String](children) { (ctx, child) =>
            val inlines = inlineValuesToTex(child.text.inline)(ctx).map(s => s"\\item[$s]")

            inlines.data +: sastToTex(child.content)(inlines)
          }.map { content =>
            "\\begin{description}" +: content :+ "\\end{description}"
          }


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


            val included = includeResolver(doc.parsed.file)
            val stack    = ctx.stack

            new SastToTexConverter(project, doc.parsed.file.parent, doc.parsed.reporter, includeResolver)
            .sastSeqToTex(included)(ctx.push(sast)).copy(stack = stack)

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
                                          .replace("\n", "\\newline{}\n")
          ctx.ret(Chain("\\noindent", latexenc))

        case other =>
          val restext = tlblock.attributes.named.get("label") match {
            case None        => text
            case Some(label) =>
              text.replaceAll(""":§([^§]*?)§""", s"""(*@\\\\label{$label$$1}@*)""")
          }
          ctx.ret(Chain(s"\\begin{lstlisting}", restext, "\\end{lstlisting}"))

      }

    case SpaceComment(_) => ctx.empty


  }

  val sectioning: Int => String = nesting => {
    val secs = Array("book", "part", "chapter", "section", "subsection", "paragraph")
    val sec  = secs.lift(nesting).getOrElse("paragraph")
    sec
  }
  def inlineValuesToTex(inners: Seq[Inline])(implicit ctx: Cta): Ctx[String] = ctx.ret(inners.map {
    case InlineText(str)                          => latexencode(str)
    case Macro(Cite, attributes)                  => s"\\cite{${attributes.target}}"
    case Macro(Code, attrs)                       => s"\\texttt{${latexencode(attrs.target)}}"
    case Macro(Comment, attributes)               => ""
    case Macro(Def, _)                            => ""
    case Macro(Emph, attrs)                       => s"\\emph{${latexencode(attrs.target)}}"
    case Macro(Label, attributes)                 => s"\\label{${attributes.target}}"
    case Macro(Math, attrs)                       => s"$$${attrs.target}${latexencode(attrs.target)}$$"
    case Macro(Other("subparagraph"), attributes) => s"\\subparagraph{${attributes.target}}"
    case Macro(Other("textsc"), attributes)       => s"\\textsc{${attributes.target}}"
    case Macro(Ref, attributes)                   => s"\\ref{${attributes.target}}"
    case Macro(Strong, attrs)                     => s"\\textbf{${latexencode(attrs.target)}}"


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


    case Macro(Other("tableofcontents"), attributes) =>
      List("\\clearpage", "\\tableofcontents*", "\\clearpage").mkString("\n")

    case im @ Macro(command, attributes) =>
      scribe.warn(s"inline macro “$command[$attributes]”")
      s"$command[${attributes.all.mkString(",")}]"
  }.mkString(""))

  def reportPos(m: Macro): String = reporter(m)

}

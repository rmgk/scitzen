package scitzen.outputs

import better.files.File
import cats.data.Chain
import scitzen.generic.Sast._
import scitzen.generic.{ConversionContext, Project, Reporter, Sast}
import scitzen.parser.MacroCommand.{Cite, Code, Comment, Def, Emph, Image, Include, Label, Link, Lookup, Math, Other, Ref, Strong}
import scitzen.parser.{Attributes, Inline, InlineText, Macro}


class SastToTexConverter(project: Project,
                         cwd: File,
                         reporter: Reporter,
                         includeResolver: Map[File, Seq[Sast]]
                        ) {

  type CtxCS = ConversionContext[Chain[String]]
  type Ctx[T] = ConversionContext[T]
  type Cta = Ctx[_]

  def convert(mainSast: List[Sast])(implicit ctx: Cta): CtxCS = {
    project.documentManager.macros.find(_.command == Other("tableofcontents"))
           .fold(Chain.empty[String])(_ => Chain("\\frontmatter")) ++:
    (mainSast match {
      case (section @ Section(title, _, _)) :: rest =>
        val ilc = inlineValuesToTex(title.inline)
        s"\\title{${ilc.data}}\\maketitle{}" +: sastSeqToTex(rest)(ilc.push(section))

      case list =>
        sastSeqToTex(mainSast)(ctx)
    })
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


  val sectioning: Int => String = nesting => {
    // "book", "part", "chapter",
    val secs = Array("", "section", "subsection", "paragraph")
    val sec  = secs.lift(nesting).getOrElse("paragraph")
    sec
  }

  def sastToTex(sast: Sast)(implicit ctx: Cta): CtxCS = sast match {

    case NoContent => ctx.empty

    case tlBlock: SBlock => blockToTex(tlBlock)

    case section @ Section(title, prefix, attr) =>
      val ilc = inlineValuesToTex(title.inline)(ctx)

      val header = attr.positional.headOption match {
        case None                              =>
          val sec = sectioning(prefix.length)
          s"\\$sec{${ilc.data}}"
        case Some("title")                     =>
          s"\\title{${ilc.data}}\\maketitle{}"
        case Some("chapter" | "part" | "book") =>
          s"\\${attr.positional.head}{${ilc.data}}"
        case other                             =>
          scribe.warn(s"invalid section type: ${other.get}" + reporter(attr.prov))
          val sec = sectioning(prefix.length)
          s"\\$sec{${ilc.data}}"
      }
      ilc.push(section).retc(header)

    case Slist(children) =>
      children match {
        case Nil => ctx.ret(Chain.nil)

        case SlistItem(m, text, NoContent | Slist(_)) :: _ =>
          "\\begin{itemize}" +:
          ctx.fold[SlistItem, String](children) { (ctx, child) =>
            val inlineCtx  = inlineValuesToTex(child.text.inline)(ctx).map(s => Chain(s"\\item{$s}"))
            val contentCtx = sastToTex(child.content)(inlineCtx)
            inlineCtx.data ++: contentCtx
          } :+
          "\\end{itemize}"

        case SlistItem(m, _, _) :: _ =>
          ctx.fold[SlistItem, String](children) { (ctx, child) =>
            val inlines = inlineValuesToTex(child.text.inline)(ctx).map(s => s"\\item[$s]{}")

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
      tlblock.attributes.positional.headOption match {
        case Some(blockname) =>
          blockname match {
            case "figure" =>
              val (figContent, caption) = {
                blockContent.lastOption match {
                  case Some(inner @ SBlock(_, Paragraph(content))) =>
                    val captionstr = inlineValuesToTex(content.inline).data
                    (blockContent.init,
                    s"\\caption{$captionstr}")
                  case other                                       =>
                    scribe.warn(s"figure has no caption" + reporter(tlblock.attributes.prov))
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
            case None        => text
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

  def inlineValuesToTex(inners: Seq[Inline])(implicit ctx: Cta): Ctx[String] = ctx.ret(inners.map {
    case InlineText(str)                    => latexencode(str)
    case Macro(Cite, attr)                  => s"${nbrs(attr)}\\cite{${attr.target}}"
    case Macro(Code, attrs)                 => s"\\texttt{${latexencode(attrs.target)}}"
    case Macro(Comment, attr)               => ""
    case Macro(Def, _)                      => ""
    case Macro(Emph, attrs)                 => s"\\emph{${latexencode(attrs.target)}}"
    case Macro(Label, attr)                 => s"\\label{${attr.target}}"
    case Macro(Math, attrs)                 => s"$$${attrs.target}$$"
    case Macro(Other("break"), attrs)       => s"\\clearpage{}"
    case Macro(Other("subparagraph"), attr) => s"\\subparagraph{${attr.target}}"
    case Macro(Other("textsc"), attr)       => s"\\textsc{${attr.target}}"
    case Macro(Other("todo"), attr)         => s"{\\color{red}TODO:${attr.target}}"
    case Macro(Ref, attr)                   => s"${nbrs(attr)}\\ref{${attr.target}}"
    case Macro(Strong, attrs)               => s"\\textbf{${latexencode(attrs.target)}}"


    case Macro(Link, attributes) =>
      val target = attributes.target
      if (attributes.positional.size > 1) {
        val name = """{"""" + attributes.positional.head + """"}"""
        s"\\href{$target}{$name}"
      }
      else s"\\url{$target}"

    case Macro(Lookup, attributes) =>
      if (project.config.definitions.contains(attributes.target))
        project.config.definitions(attributes.target)
      else scribe.warn(s"unknown name ${attributes.target}" + reporter(attributes.prov))

    case Macro(Other("footnote"), attributes) =>
      val target = latexencode(attributes.target)
      s"\\footnote{$target}"


    case Macro(Other("tableofcontents"), attributes) =>
      List("\\clearpage", "\\tableofcontents*", "\\clearpage", "\\mainmatter").mkString("\n")

    case im @ Macro(Other(command), attributes) =>
      val str = SastToScimConverter().macroToScim(im)
      scribe.warn(s"unknown macro “$str”" + reporter(im))
      str

    case im @ Macro(Image | Include, attributes) =>
      val str = SastToScimConverter().macroToScim(im)
      scribe.warn(s"tex backend does not allow inline images or includes" + reporter(im))
      str
  }.mkString(""))
}

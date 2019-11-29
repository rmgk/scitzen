package scitzen.outputs

import better.files.File
import cats.data.Chain
import scitzen.generic.Sast._
import scitzen.generic.{ConversionContext, Project, Sast, Scope}
import scitzen.parser.MacroCommand.{Cite, Comment, Def, Image, Include, Label, Link, Other, Quote, Ref}
import scitzen.parser.{Inline, InlineText, Macro}


object TypeAliasses {
    type CtxCS = ConversionContext[Chain[String]]
  type Ctx[T] = ConversionContext[T]
  type CS = Chain[String]
  type Cta = Ctx[_]
}


class SastToTexConverter(project: Project,
                         currentFile: File,
                         numbered: Boolean = true
                        ) {

import scitzen.outputs.TypeAliasses._

  def convert(mainSast: List[Sast])(implicit ctx: Cta): CtxCS = mainSast match {
    case List(Section(title, content, _)) =>
      val ilc = inlineValuesToTex(title.inline)
      s"\\title{${ilc.data}}\\maketitle{}" +:
      sastSeqToTex(content)(ilc.copy(scope = new Scope(2)))

    case list => sastSeqToTex(list)(ctx.copy(scope = new Scope(1)))
  }

  def latexencode(input: String): String = {
    val nobs = input.replaceAllLiterally("\\", "»ℓ§«")
    val nosimple = "&%$#_{}".toList.map(_.toString).foldLeft(nobs) { (acc, char) =>
      acc.replaceAllLiterally(char,  s"\\$char")
    }
    val nomuch = List("~" -> "\\textasciitilde{}",
    "^" -> "\\textasciicircum{}").foldLeft(nosimple){case (acc, (char, rep)) =>
      acc.replaceAllLiterally(char, rep)
    }
    nomuch.replaceAllLiterally("»ℓ§«", "\\textbackslash{}")
  }

  def sastSeqToTex(b: Seq[Sast])(implicit ctx: Cta): CtxCS = {
    ctx.fold(b) { (ctx, sast) => sastToTex(sast)(ctx) }
  }

  def sastToTex(sast: Sast)(implicit ctx: Cta): CtxCS = sast match {

      case tlBlock: TLBlock => blockToTex(tlBlock)

      case Section(title, contents, _) =>
        val sec = sectioning(ctx.scope)
        val ilc = inlineValuesToTex(title.inline)
        s"\\$sec{${ilc.data}}" +:
        sastSeqToTex(contents)(ilc.copy(scope = ctx.scope.inc))

      case Slist(children) =>
        children match {
          case Nil => ctx.ret(Chain.nil)

          case SlistItem(m, _) :: _ if m.contains(":") =>
            ctx.fold(children) { (ctx, child) =>
              s"\\item[${child.marker.replaceAllLiterally(":", "")}]" +:
              sastSeqToTex(child.content)(ctx)
            }.map { content =>
              "\\begin{description}" +: content :+ "\\end{description}"
            }

          case other =>
            "\\begin{itemize}" +:
            ctx.fold(children) { (ctx, child) =>
              s"\\item" +: sastSeqToTex(child.content)(ctx)
            } :+
            "\\end{itemize}"
        }

      case MacroBlock(mcro) => mcro match {
        case Macro(Image, attributes) =>
          val target    = attributes.target
          val imagepath = ctx.image(currentFile, target)
          if (imagepath.data.isEmpty) {
            scribe.error(s"Not relative path: $mcro")
            ctx.empty
          }
          else {
            ctx.ret(Chain(s"\\noindent{}\\includegraphics[width=\\columnwidth]{${imagepath.data.get}}\n"))
          }

        case Macro(Include, attributes) =>
          ImportPreproc.macroImportPreproc(project.findDoc(currentFile.parent, attributes.target), attributes) match {
            case Some((doc, sast)) =>
              new SastToTexConverter(project, doc.file.parent, numbered)
              .sastSeqToTex(sast)(ctx.copy(scope = new Scope(3)))

            case None => ctx.empty
          }

        case other =>
          inlineValuesToTex(List(other)).single
      }
  }


  def blockToTex(tlblock: TLBlock)(implicit ctx: Cta): CtxCS = tlblock.content match {
      case Paragraph(content) => inlineValuesToTex(content.inline).single :+ ""


      case ParsedBlock(delimiter, blockContent) =>
        delimiter.charAt(0) match {
          case '=' => sastSeqToTex(blockContent)
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case ' ' => sastSeqToTex(blockContent)
          // there is also '=' example, and '+' passthrough.
          // examples seems rather specific, and passthrough is not implemented.
          case _   => sastSeqToTex(blockContent)
        }

      case RawBlock(delimiter, text) =>
        if (tlblock.attr.named.contains("converter"))
          sastSeqToTex(ctx.convert(tlblock, "pdf"))
        else if (delimiter.isEmpty || delimiter == "comment|space") ctx.empty
        else delimiter.charAt(0) match {
          case '`' =>
            ctx.ret(Chain(s"\\begin{verbatim}", text, "\\end{verbatim}"))
          case '.' =>
            val latexenc = latexencode(text).trim
                           .replaceAll("\n{2,}", """\\newline{}\\noindent{}""")
                            .replaceAllLiterally("\n", "\\newline{}\n")
            ctx.ret(Chain("\\noindent",latexenc))
        }




    }

  val sectioning:  Scope => String = nesting => {
    val secs = Array("book", "part", "chapter", "section", "subsection", "paragraph")
    val sec = secs.lift(nesting.level).getOrElse("paragraph")
    if (numbered) sec else sec + "*"
  }
  def inlineValuesToTex(inners: Seq[Inline])(implicit ctx: Cta): Ctx[String] = ctx.ret(inners.map {
    case InlineText(str) => latexencode(str)
    case Macro(Def, _) => ""
    case Macro(Quote(q), inner2) =>
      val inner = latexencode(inner2.target)
      //scribe.warn(s"inline quote $q: $inner; ${post.sourcePath}")
      q.head match {
        case '_' => s"\\emph{$inner}"
        case '*' => s"\\textbf{$inner}"
        case '`' => s"\\texttt{$inner}"
        case '$' => s"$$${inner2.target}$$"
      }
    case Macro(Comment, attributes) => ""
    case Macro(Ref, attributes)      => s"\\ref{${latexencode(attributes.target)}}"
    case Macro(Cite, attributes)              =>
      s"\\cite{${attributes.target}}"
    case Macro(Link, attributes)              =>
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
    case Macro(Label, attributes)             => s"\\label{${attributes.target}}"
    case Macro(Other("tableofcontents"), attributes) =>
      List("\\clearpage", "\\tableofcontents*", "\\clearpage").mkString("\n")
    case im @ Macro(command, attributes)      =>
      scribe.warn(s"inline macro “$command[$attributes]”")
      s"$command[${attributes.all.mkString(",")}]"
  }.mkString(""))
}

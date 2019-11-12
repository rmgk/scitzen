package scitzen.outputs

import better.files.File
import scitzen.generic.Sast._
import scitzen.generic.{DocumentManager, ImageResolver, Sast}
import scitzen.parser.MacroCommand.{Cite, Comment, Image, Include, Label, Link, Other, Quote, Ref}
import scitzen.parser.{Inline, InlineText, Macro}

class Scope(val level: Int) extends AnyVal {
  def inc: Scope = {
    new Scope(level + 1)
  }
}


class SastToTexConverter(documents: DocumentManager,
                         root: File,
                         numbered: Boolean = true,
                         imageResolver: ImageResolver) {

  def convert(mainSast: List[TLBlock]): Seq[String] = mainSast match {
    case List(TLBlock(_, Section(title, content))) =>
      val secChildren = content.collect { case TLBlock(_, s: Section) => s }
      s"\\title{${inlineValuesToTex(title.inline)}}\\maketitle{}" +:
      (putAbstract(content.map(_.content)) ++ sastToTex(secChildren)(scope = new Scope(2)))
    case list                                         => cBlocks(list)(scope = new Scope(1))
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

  def putAbstract(contents: Seq[Sast]): Seq[String] = {
    val secContent = contents.filter(!_.isInstanceOf[Section])
      "\\begin{abstract}" +:
      sastToTex(secContent)(new Scope(1)) :+
      "\\end{abstract}"
  }

  def cBlocks(b: Seq[TLBlock])(implicit scope: Scope): Seq[String] = {
    b.flatMap { bwa: TLBlock =>
      val positiontype = bwa.attr.positional.headOption
      positiontype match {
        case _ => sastToTex(List(bwa.content))
      }
    }
  }

  def sastToTex(b: Seq[Sast])(implicit scope: Scope): Seq[String] = {
    b.flatMap {

      case AttributeDef(_) => Nil


      case Section(title, contents) =>
        val sec = sectioning(scope)
        s"\\$sec{${inlineValuesToTex(title.inline)}}" +:
        cBlocks(contents)(scope.inc)

      case Slist(children) =>
        children match {
          case Nil => Nil
          case SlistItem(m, _) :: _ if m.contains(":") =>
            "\\begin{description}" +:
            children.flatMap { child =>
              s"\\item[${child.marker.replaceAllLiterally(":", "")}]" +: sastToTex(child.content)
            } :+
            "\\end{description}"
          case other =>
            "\\begin{itemize}" +:
            children.flatMap { child =>
              s"\\item" +: sastToTex(child.content)
            } :+
            "\\end{itemize}"
        }

      case MacroBlock(mcro) => mcro match {
        case Macro(Image, attributes) =>
          val target = attributes.target
          val imagepath = imageResolver.image(root, target)
          if (imagepath.endsWith(".gif") || imagepath.endsWith(".webp")) {
            scribe.warn(s"tex output currently does not support '$imagepath', omitting")
            List()
          }
          else List(s"\\noindent{}\\includegraphics[width=\\columnwidth]{$imagepath}\n")

        case Macro(Include, attributes) =>
          ImportPreproc.macroImportPreproc(documents.find(root, attributes.target), attributes) match {
            case Some((doc, sast)) =>
              new SastToTexConverter(documents, doc.file.parent, numbered, imageResolver)
              .cBlocks(sast)(new Scope(3))
            case None => Nil
          }
        case other                        =>
          List(inlineValuesToTex(List(other)))
      }

      case Paragraph(content) => List(inlineValuesToTex(content.inline)) :+ ""


      case ParsedBlock(delimiter, blockContent) =>
        delimiter.charAt(0) match {
          case '=' => cBlocks(blockContent)
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case ' ' => cBlocks(blockContent)
          // there is also '=' example, and '+' passthrough.
          // examples seems rather specific, and passthrough is not implemented.
          case _   => cBlocks(blockContent)
        }

      case RawBlock(delimiter, text) =>
        if (delimiter.isEmpty || delimiter == "comment|space") Nil
        else delimiter.charAt(0) match {
          case '`' =>
            List(s"\\begin{verbatim}", text, "\\end{verbatim}")
          case '.' =>
            val latexenc = latexencode(text).trim
                           .replaceAll("\n{2,}", """\\newline{}\\noindent{}""")
                            .replaceAllLiterally("\n", "\\newline{}\n")
            List("\\noindent",latexenc)
        }




    }
  }

  val sectioning:  Scope => String = nesting => {
    val secs = Array("book", "part", "chapter", "section", "subsection", "paragraph")
    val sec = secs.lift(nesting.level).getOrElse("paragraph")
    if (numbered) sec else sec + "*"
  }
  def inlineValuesToTex(inners: Seq[Inline]): String = inners.map[String, Seq[String]] {
    case InlineText(str) => latexencode(str)
    case Macro(Quote(q), inner2) =>
      val inner = latexencode(inner2.target)
      //scribe.warn(s"inline quote $q: $inner; ${post.sourcePath}")
      q.head match {
        case '_' => s"\\emph{$inner}"
        case '*' => s"\\textbf{$inner}"
        case '`' => s"\\texttt{$inner}"
        case '$' => s"$$$inner$$"
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
    case Macro(Other("footnote"), attributes) =>
      val target = latexencode(attributes.target)
      s"\\footnote{$target}"
    case Macro(Label, attributes)             => s"\\label{${attributes.target}}"
    case im @ Macro(command, attributes)      =>
      scribe.warn(s"inline macro “$command[$attributes]”")
      s"$command[${attributes.all.mkString(",")}]"
  }.mkString("")
}

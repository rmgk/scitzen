package scitzen.outputs

import better.files.File
import scitzen.parser.{Inline, InlineQuote, InlineText, Macro}
import scitzen.generic.{DocumentManager, ImageResolver, Sast}
import scitzen.generic.Sast._

class Scope(val level: Int) extends AnyVal {
  def inc: Scope = {
    new Scope(level + 1)
  }
}


class SastToTexConverter(documents: DocumentManager,
                         root: File,
                         numbered: Boolean = true,
                         imageResolver: ImageResolver) {

  def convert(): Seq[String]= {
    documents.mainSast() match {
      case List(Section(title, content)) =>
        val secChildren = content.collect{case s: Section => s}
        s"\\title{${inlineValuesToHTML(title.inline)}}" +:
        (putAbstract(content) ++ sastToTex(secChildren)(scope = new Scope(3)))
      case list => sastToTex(list)(scope = new Scope(1))

    }
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


  def sastToTex(b: Seq[Sast])(implicit scope: Scope): Seq[String] = {
    b.flatMap {

      case AttributeDef(_) => Nil

      case Text(inner) => List(inlineValuesToHTML(inner))

      case Section(title, contents) =>
        val sec = sectioning(scope)
        s"\\$sec{${inlineValuesToHTML(title.inline)}}" +:
        sastToTex(contents)(scope.inc)

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
        case Macro("image", attributes) =>
          val target = attributes.target
          val imagepath = imageResolver.image(root, target)
          if (imagepath.endsWith(".gif") || imagepath.endsWith(".webp")) {
            scribe.warn(s"tex output currently does not support '$imagepath', omitting")
            List()
          }
          else List(s"\\noindent{}\\includegraphics[width=\\columnwidth]{$imagepath}\n")

        case Macro("label", attributes) => List(s"\\label{${attributes.target}}")
        case Macro("include", attributes) =>
          val docOpt = documents.find(attributes.target)
          docOpt.toList.flatMap { doc =>
            val date = doc.sdoc.date.fold("")(d => d.date.full + " ")
            val section = doc.sast.head.asInstanceOf[Section]
            val sast = section.copy(title = Text(InlineText(date) +: section.title.inline))
            new SastToTexConverter(documents, doc.file.parent, numbered, imageResolver)
            .sastToTex(List(sast))(new Scope(3))
          }
        case other                        =>
          scribe.warn(s"not implemented: $other")
          List(other.toString)
      }

      case ParsedBlock(delimiter, blockContent) =>
        if (delimiter == "") sastToTex(blockContent) :+ ""
        else delimiter.charAt(0) match {
          case '=' => sastToTex(blockContent)
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case ' ' => sastToTex(blockContent)
          // there is also '=' example, and '+' passthrough.
          // examples seems rather specific, and passthrough is not implemented.
          case _   => sastToTex(blockContent)
        }

      case RawBlock(delimiter, text) =>
        if (delimiter.isEmpty) Nil
        else delimiter.charAt(0) match {
          case '`' =>
            List(s"\\begin{verbatim}", text, "\\end{verbatim}")
          case '.' =>
            val latexenc = latexencode(text).trim
                           .replaceAll("\n{2,}", """\\newline{}\\noindent{}""")
                            .replaceAllLiterally("\n", "\\newline{}\n")
            List("\\noindent",latexenc)
        }



      case bwa: AttributedBlock =>
        val positiontype = bwa.attr.attributes.positional.headOption
        positiontype match {
          case _         => sastToTex(List(bwa.content))
        }
    }
  }

  val sectioning:  Scope => String = nesting => {
    val secs = Array("book", "part", "chapter", "section", "subsection", "paragraph")
    val sec = secs(nesting.level)
    if (numbered) sec else sec + "*"
  }
  def inlineValuesToHTML(inners: Seq[Inline]): String = inners.map {
    case InlineText(str) => latexencode(str)
    case InlineQuote(q, inner2) =>
      val inner = latexencode(inner2)
      //scribe.warn(s"inline quote $q: $inner; ${post.sourcePath}")
      q.head match {
        case '_' => s"\\emph{$inner}"
        case '*' => s"\\textbf{$inner}"
        case '`' => s"\\texttt{$inner}"
        case '$' => s"$$$inner$$"
      }
    case Macro("comment", attributes) => ""
    case Macro("ref", attributes) => s"\\ref{${latexencode(attributes.target)}}"
    case Macro("cite", attributes) =>
      s"\\cite{${latexencode(attributes.target)}}"
    case Macro("link", attributes) =>
      val target = attributes.target
      if (attributes.positional.size > 1) {
        val name = """{"""" + attributes.positional.head + """"}"""
        s"\\href{$target}{$name}"
      }
      else s"\\url{$target}"
    case Macro("footnote", attributes) =>
      val target = latexencode(attributes.target)
      s"\\footnote{$target}"
    case im @ Macro(command, attributes) =>
      scribe.warn(s"inline macro “$command[$attributes]”")
      s"$command[${attributes.all.mkString(",")}]"
  }.mkString("")
}

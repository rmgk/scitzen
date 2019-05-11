package scitzen.converter

import scitzen.cli.DocumentManager
import scitzen.parser.{Attributes, Inline, InlineQuote, InlineText, Macro}
import scitzen.semantics.Sast
import scitzen.semantics.Sast._

class NestingLevel(val i: Int) extends AnyVal {
  def inc: NestingLevel = {
    new NestingLevel(i + 1)
  }
}


class SastToTexConverter(documents: DocumentManager,
                         numbered: Boolean = true,
                         reldir: String = "",
                         imagemap: Map[String, String] = Map()) {
  val reldir2 = if(reldir.isEmpty) "" else reldir +"/"

  def convert(): Seq[String]= {
    documents.mainSast() match {
      case List(Section(title, content)) =>
        val secChildren = content.collect{case s: Section => s}
        s"\\title{${inlineValuesToHTML(title.inline)}}" +:
        (putAbstract(content) ++ sastToTex(secChildren)(nestingLevel = new NestingLevel(2)))
      case list => sastToTex(list)

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
      sastToTex(secContent) :+
      "\\end{abstract}"
  }


  def sastToTex(b: Seq[Sast])(implicit nestingLevel: NestingLevel = new NestingLevel(1)): Seq[String] = {
    b.flatMap {

      case AttributeDef(_) => Nil

      case Text(inner) => List(inlineValuesToHTML(inner))

      case Section(title, contents) =>
        val sec = sectioning(nestingLevel)
        s"\\$sec{${inlineValuesToHTML(title.inline)}}" +:
        sastToTex(contents)(nestingLevel.inc)

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
          val imagepath = imagemap.getOrElse(s"$reldir2$target", target)
          scribe.info(s"resolving $target in $reldir, resulted in $imagepath")

          println()
          List(
            s"\\noindent{}\\includegraphics[width=\\columnwidth]{$imagepath}\\newline{}",
            )

        case Macro("label", attributes) => List(s"\\label{${attributes.target}}")
        case Macro("include", attributes) =>
          val docOpt = documents.find(attributes.target)
          docOpt.toList.flatMap{doc =>
            val date = doc.sdoc.date.fold("")(d => d.date.full + " ")
            val section = doc.sast.head.asInstanceOf[Section]
            val sast = section.copy(title = Text(InlineText(date) +: section.title.inline ))
            sastToTex(List(sast))(new NestingLevel(3))}
        case other =>
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
          case '`'  =>
            List(s"\\begin{verbatim}", text, "\\end{verbatim}")
          case '.' => List("\\newline{}\n\\noindent", latexencode(text).replaceAllLiterally("\n", "\\newline{}\n"))
        }



      case bwa: AttributedBlock =>
        val positiontype = bwa.attr.attributes.positional.headOption
        positiontype match {
          case _         => sastToTex(List(bwa.content))
        }
    }
  }

  val sectioning:  NestingLevel => String = nesting => {
    val sec = nesting.i match {
      case 1 => "part"
      case 2 => "chapter"
      case 3 => "section"
      case 4 => "subsection"
      case 5 => "paragraph"
    }
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
      linkTo(attributes, target)
    case Macro("footnote", attributes) =>
      val target = latexencode(attributes.target)
      s"\\footnote{$target}"
    case im @ Macro(command, attributes) =>
      scribe.warn(s"inline macro “$command[$attributes]”")
      s"$command[${attributes.all.mkString(",")}]"
  }.mkString("")
  def linkTo(attributes: Attributes, linktarget: String): String = {
    s"\\url{${linktarget}}"
  }
}

package scitzen.converter

import scitzen.parser.{Attribute, Inline, InlineQuote, InlineText, Macro}
import scitzen.semantics.Sast
import scitzen.semantics.Sast._
import scitzen.semantics.SastAnalyzes.AnalyzeResult

class NestingLevel(val i: Int) extends AnyVal {
  def inc: NestingLevel = {
    new NestingLevel(i + 1)
  }
}


class SastToTexConverter(analyzeResult: AnalyzeResult,
                         reldir: String = "",
                         imagemap: Map[String, String] = Map()) {
  val reldir2 = if(reldir.isEmpty) "" else reldir +"/"

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



  def sastToTex(b: Seq[Sast])(implicit nestingLevel: NestingLevel = new NestingLevel(1)): Seq[String] = {
    b.flatMap {

      case AttributeDef(_) => Nil

      case Text(inner) => List(inlineValuesToHTML(inner))

      case Section(title, contents) =>
        val sec = sectioning(nestingLevel)

        def rec(block: Seq[Sast]): Seq[String] = sastToTex(block)(nestingLevel.inc)

        def putAbstract: Seq[String] = {
          val secContent = contents.filter(!_.isInstanceOf[Section])
          val secChildren = contents.collect{case s: Section => s}
          (if (analyzeResult.named.getOrElse("layout", "").contains("acm")) {
          "\\begin{abstract}" +:
           rec(secContent) :+
           "\\end{abstract}" :+
           "\\maketitle"
          } else {
            "\\maketitle" +:
            "\\begin{abstract}" +:
            rec(secContent) :+
            "\\end{abstract}"
          }) ++ rec(secChildren)
        }

        s"\\$sec{${inlineValuesToHTML(title.inline)}}" +:
        (if (nestingLevel.i == 1) {
          putAbstract
        } else rec(contents))

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
          val target = attributes.last.value
          val imagepath = imagemap.getOrElse(s"$reldir2$target", target)
          scribe.info(s"resolving $target in $reldir, resulted in $imagepath")

          println()
          List(
            s"\\newline{}\\includegraphics[width=\\columnwidth]{$imagepath}\\newline{}",
            )

        case Macro("label", attributes) => List(s"\\label{${attributes.last.value}}")
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
          case '.' => List(latexencode(text).replaceAllLiterally("\n", "\\newline{}\n"))
        }



      case bwa: AttributedBlock =>
        val positiontype = bwa.attr.positional.headOption
        positiontype match {
          case _         => sastToTex(List(bwa.content))
        }
    }
  }

  val sectioning:  NestingLevel => String =
    if (analyzeResult.named.getOrElse("layout", "").contains("thesis")) { _.i match {
      case 1 => "title"
      case 2 => "chapter"
      case 3 => "section"
      case 4 => "subsection"
      case 5 => "subsubsection"
    }}
    else { _.i match {
      case 1 => "title"
      case 2 => "section"
      case 3 => "subsection"
      case 4 => "paragraph"
    }
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
    case Macro("ref", attributes) => s"\\ref{${latexencode(attributes.last.value)}}"
    case Macro("cite", attributes) =>
      s"\\cite{${latexencode(attributes.last.value)}}"
    case Macro("link", attributes) =>
      val target = latexencode(attributes.last.value)
      linkTo(attributes, target)
    case Macro("footnote", attributes) =>
      val target = latexencode(attributes.last.value)
      s"\\footnote{$target}"
    case im @ Macro(command, attributes) =>
      scribe.warn(s"inline macro “$command[$attributes]”")
      s"$command[${attributes.mkString(",")}]"
  }.mkString("")
  def linkTo(attributes: Seq[Attribute], linktarget: String): String = {
    s"\\url{${latexencode(linktarget)}}"
  }
}

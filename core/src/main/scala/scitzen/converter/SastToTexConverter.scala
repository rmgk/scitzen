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


class SastToTexConverter(analyzeResult: AnalyzeResult) {

  def sastToTex(b: Sast)(implicit nestingLevel: NestingLevel = new NestingLevel(1)): Seq[String] = {
    b match {

      case Sseqf(inner) => inner.flatMap(sastToTex)

      case AttributeDef(_) => Nil

      case Text(inner) => List(inlineValuesToHTML(inner))

      case Section(title, secContent) =>
        val sec = nestingLevel.i match {
          case 1 => "title"
          case 2 => "section"
          case 3 => "subsection"
          case 4 => "paragraph"
        }
        s"\\$sec{${inlineValuesToHTML(title.inline)}}" +:
        (if (nestingLevel.i == 1) "\\maketitle" else "") +:
        sastToTex(secContent)(nestingLevel.inc)

      case Slist(children) =>
        if (children.isEmpty) Nil
        else {
          "\\begin{itemize}" +:
          children.flatMap { child =>
            s"\\item ${sastToTex(child.content).mkString("")}" +:
            sastToTex(child.inner)
          } :+
          "\\end{itemize}"
        }

      case MacroBlock(mcro) => mcro match {
        case Macro("image", attributes) =>
          val target = attributes.last.value
          List(s"\\includegraphics{$target}")

        case Macro("label", attributes) => List(s"\\label{${attributes.last.value}}")
        case other =>
          scribe.warn(s"not implemented: $other")
          List(other.toString)
      }

      case ParsedBlock(delimiter, blockContent) =>
        if (delimiter == "") sastToTex(blockContent)
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
        delimiter.charAt(0) match {
          case '`' | '.' =>
            List(s"\\begin{lstlisting}", text, "\\end{lstlisting}")
        }



      case bwa: AttributedBlock =>
        val positiontype = bwa.attr.positional.headOption
        positiontype match {
          case _         => sastToTex(bwa.content)
        }
    }
  }

  def inlineValuesToHTML(inners: Seq[Inline]): String = inners.map {
    case InlineText(str) => str
    case InlineQuote(q, inner) =>
      //scribe.warn(s"inline quote $q: $inner; ${post.sourcePath}")
      q.head match {
        case '_' => s"\\emph{$inner}"
        case '*' => s"\\textbf{$inner}"
        case '`' => s"\\code{$inner}"
        case '$' => s"$$$inner$$"
      }
    case Macro("//", attributes) => ""
    case Macro("ref", attributes) => s"\\ref{${attributes.last.value}}"
    case Macro("cite", attributes) =>
      s"\\cite{${attributes.last.value}}"
    case Macro("link", attributes) =>
      val target = attributes.last.value
      linkTo(attributes, target)
    case Macro("footnote", attributes) =>
      val target = attributes.last.value
      s"\\footnote{$target}"
    case im @ Macro(command, attributes) =>
      scribe.warn(s"inline macro “$command[$attributes]”")
      s"$command[${attributes.mkString(",")}]"
  }.mkString("")
  def linkTo(attributes: Seq[Attribute], linktarget: String): String = {
    s"\\url{$linktarget}"
  }
}

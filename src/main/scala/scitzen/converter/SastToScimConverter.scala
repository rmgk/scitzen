package scitzen.converter

import scitzen.parser.{Attribute, Inline, InlineQuote, InlineText, Macro}
import scitzen.semantics.Sast
import scitzen.semantics.Sast._


object SastToScimConverter {


  def attributesToScim(attributes: Seq[Attribute]): String = attributes.map {
    case Attribute("", v) if !v.contains(";") => v
    case Attribute("", v)                     => s""""$v""""
    case Attribute(k, v)                      => s"""$k="$v""""
  }.mkString("[", ";", "]")

  def toScim(b: Sast)(implicit nestingLevel: NestingLevel = new NestingLevel(1)): Seq[String] = {
    b match {

      case Sseqf(inner) => inner.flatMap(toScim)

      case AttributeDef(a) => List(s":${a.id}:${a.value}")

      case Text(inner) => List(inlineToScim(inner))

      case Section(title, secContent, secChildren) =>
        def rec(block: Seq[Sast]): Seq[String] = block.flatMap(toScim(_)(nestingLevel.inc))

        ("=" * nestingLevel.i + " " + title) +:
        (rec(secContent) ++ rec(secChildren))

      case Slist(children) => children.flatMap {
        case SlistItem(marker, content, inner) =>
          s"$marker" +: (toScim(content) ++ toScim(inner))
      }

      case MacroBlock(mcro) => List(macroToScim(mcro))

      case ParsedBlock(delimiter, blockContent) =>
        if (delimiter == "") toScim(blockContent)
        else delimiter.charAt(0) match {
          case '=' => delimiter +: toScim(blockContent) :+ delimiter
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case ' ' => toScim(blockContent).map(line => s"$delimiter$line")
        }

      case RawBlock(delimiter, text) => List(delimiter, text, delimiter)


      case bwa: AttributedBlock =>
        bwa.attr.rawAttributes.map(attributesToScim) ++ toScim(bwa.content)
    }
  }


  private def macroToScim(mcro: Macro): String = {
    s":${mcro.command}${attributesToScim(mcro.attributes)}"
  }
  def inlineToScim(inners: Seq[Inline]): String = inners.map {
    case InlineText(str)        => str
    case InlineQuote(q, inner2) => s":$q$inner2$q"
    case m: Macro               => macroToScim(m)
  }.mkString("")
}

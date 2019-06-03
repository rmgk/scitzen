package scitzen.outputs

import scitzen.parser.{Attribute, Inline, InlineQuote, InlineText, Macro}
import scitzen.generic.Sast
import scitzen.generic.Sast._


case class SastToScimConverter() {


  def attributesToScim(attributes: Seq[Attribute]): String = attributes.map {
    case Attribute("", v) if !(v.contains(";") || v.contains("\n")) => v
    case Attribute("", v)                       => s""""$v""""
    case Attribute(k, v)                        => s"""$k="$v""""
  }.mkString("[", "; ", "]")

  def toScim(blocks: Seq[TLBlock])(implicit nestingLevel: Scope = new Scope(1)): Seq[String] = {
    blocks.flatMap { bwa =>
      bwa.attr.raw.map(attributesToScim) ++ toScimS(List(bwa.content))
    }
  }

    def toScimS(b: Seq[Sast])(implicit nestingLevel: Scope = new Scope(1)): Seq[String] = {
    b.flatMap[String, Seq[String]] {

      case AttributeDef(a) => List(s":${a.id}:${a.value}")

      //case Sections(secContent, secChildren) =>
      //  def rec(block: Seq[Sast]): Seq[String] = block.flatMap(toScim(_))
      //  rec(secContent) ++ rec(secChildren)


      case Section(title, sc) =>
        ("=" * nestingLevel.level + " " + inlineToScim(title.inline)) +:
        toScim(sc)(nestingLevel.inc)

      case Slist(children) => children.flatMap {
        case SlistItem(marker, List(Paragraph(Text(inl)))) =>
          List(s"$marker" + inlineToScim(inl))
        case SlistItem(marker, inner) =>
          marker +: toScimS(inner)
      }

      case MacroBlock(Macro("horizontal-rule", attributes)) => List(attributes.target)
      case MacroBlock(mcro) => List(macroToScim(mcro))

      case Paragraph(content) => List(inlineToScim(content.inline))

      case ParsedBlock(delimiter, blockContent) =>
        delimiter.charAt(0) match {
          case '=' => delimiter +: toScim(blockContent) :+ delimiter
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case ' ' | '\t' => toScim(blockContent).iterator
                             .flatMap(line => line.split("\n", -1))
                             .map{
                               case line if line.forall(_.isWhitespace) => ""
                               case line => s"$delimiter$line"
                             }.toList
        }

      case RawBlock("comment", text) => List.fill(text.count(_ == '\n'))("")
      case RawBlock(delimiter, text) => List(delimiter, text, delimiter)

    }
  }


  def macroToScim(mcro: Macro): String = {
    s":${mcro.command}${attributesToScim(mcro.attributes.all)}"
  }

  def inlineToScim(inners: Seq[Inline]): String = inners.map {
    case InlineText(str)        => str
    case InlineQuote(q, inner2) => s":$q$inner2$q"
    case m: Macro               => macroToScim(m)
  }.mkString("")
}

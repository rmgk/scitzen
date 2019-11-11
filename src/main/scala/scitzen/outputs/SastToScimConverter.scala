package scitzen.outputs

import scitzen.generic.Sast
import scitzen.generic.Sast._
import scitzen.parser.MacroCommand.{Comment, Other, Quote}
import scitzen.parser.{Attribute, Inline, InlineText, Macro, MacroCommand}


case class SastToScimConverter() {

  val attributeEscapes = """[;\]\n]|^[\s'"]|\s$""".r
  val countQuotes = """([']+)""".r

  def encodeValue(v: String): String = {
    if (attributeEscapes.pattern.matcher(v).find()) {
      val mi = countQuotes.findAllIn(v)
      val quoteCount = if (mi.isEmpty) 0 else mi.map(_.length).max
      val quotes = "\'" * quoteCount
      s"""$quotes"$v"$quotes"""
    }
    else v
  }

  def attributesToScim(attributes: Seq[Attribute]): String = attributes.map {
    case Attribute("", v) => encodeValue(v)
    case Attribute(k, v)  => s"""$k=${encodeValue(v)}"""
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
        case SlistItem(marker, Paragraph(Text(inl)) :: rest) =>
          (s"$marker" + inlineToScim(inl)) +: toScimS(rest)
        case SlistItem(marker, inner) =>
          marker +: toScimS(inner)
      }

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

      case RawBlock("comment|space", text) => {
        text.stripLineEnd.split("\\n", -1).map(_.trim).toList
      }
      case RawBlock(delimiter, text) => List(delimiter, text, delimiter)

    }
  }


  def macroToScimRaw(mcro: Macro): String = {
    s":${MacroCommand.print(mcro.command)}${attributesToScim(mcro.attributes.all)}"
  }

  def inlineToScim(inners: Seq[Inline]): String = inners.map {
    case InlineText(str)         => str
    case m: Macro                => macroToScim(m)
  }.mkString("")

  def macroToScim(m: Macro): String = m match {
    case Macro(Other("horizontal-rule"), attributes) => attributes.target
    case Macro(Comment, attributes) => s":%${attributes.target}\n"
    case Macro(Quote(q), inner2) => s":$q${inner2.target}$q"
    case other => macroToScimRaw(other)
  }
}

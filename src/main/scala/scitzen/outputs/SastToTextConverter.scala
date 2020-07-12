package scitzen.outputs

import scitzen.parser.MacroCommand.Lookup
import scitzen.parser.Sast._
import scitzen.parser.{Inline, InlineText, Sast}

case class SastToTextConverter(definitions: Map[String, String] = Map.empty) {

  def convert(b: Seq[Sast]): Seq[String] = {
    b.flatMap {
      case Section(title, level, _) =>
        List(convertInline(title.inline))

      case Slist(children) => children.flatMap {
          case ListItem(marker, Text(inl), None) =>
            List(convertInline(inl))
          case ListItem(marker, text, Some(inner)) =>
            convertInline(text.inline) +: convert(List(inner))
        }

      case Macro(Lookup, attr) =>
        if (!definitions.contains(attr.target)) scribe.error(s"could not resolve ${attr.target}")
        List(definitions(attr.target))
      case Macro(_, _) => Nil

      case Block(_, blockType) => blockType match {
          case Paragraph(content)      => List(convertInline(content.inline))
          case Parsed(_, blockContent) => convert(blockContent)
          case Fenced(text)            => List(text)
          case SpaceComment(_)         => Nil
        }

    }
  }

  def convertInline(inners: Seq[Inline]): String =
    inners.map {
      case InlineText(str) => str
      case Macro(Lookup, attr) =>
        definitions(attr.target)
      case m: Macro        => ""
    }.mkString("")
}

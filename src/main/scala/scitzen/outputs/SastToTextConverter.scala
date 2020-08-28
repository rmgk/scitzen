package scitzen.outputs

import scitzen.sast.MacroCommand.Lookup
import scitzen.sast.{
  Block, Fenced, Inline, InlineText, ListItem, Macro, Paragraph, Parsed, Sast, Section, Slist, SpaceComment, Text
}

case class SastToTextConverter(definitions: Map[String, String] = Map.empty) {

  def convert(b: Seq[Sast]): Seq[String] = {
    b.flatMap {
      case Section(title, _, _) =>
        List(convertInline(title.inl))

      case Slist(children) => children.flatMap {
          case ListItem(_, Text(inl), None) =>
            List(convertInline(inl))
          case ListItem(_, text, Some(inner)) =>
            convertInline(text.inl) +: convert(List(inner))
        }

      case Macro(Lookup, attr) =>
        if (!definitions.contains(attr.target)) scribe.error(s"could not resolve ${attr.target}")
        List(definitions(attr.target))
      case Macro(_, _) => Nil

      case Block(_, blockType) => blockType match {
          case Paragraph(content)      => List(convertInline(content.inl))
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
      case _: Macro => ""
    }.mkString("")
}

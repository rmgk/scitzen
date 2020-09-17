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

      case Block(attr, blockType) =>
        val filterBlock = attr.positional match {
          case "if" :: parameter :: _ =>
            val res = definitions.get(parameter) match {
              case Some(value) =>
                attr.named.get("equals").forall(_ == value)
              case None => false
            }
            if (attr.named.contains("not")) !res else res
          case _ => true
        }

        if (!filterBlock) Nil
        else {
          blockType match {
            case Paragraph(content)      => List(convertInline(content.inl))
            case Parsed(_, blockContent) => convert(blockContent)
            case Fenced(text)            => List(text)
            case SpaceComment(_)         => Nil
          }
        }

    }
  }

  def convertInline(inners: Seq[Inline]): String =
    inners.map {
      case InlineText(str) => str
      case Macro(Lookup, attr) =>
        definitions.get(attr.target).orElse(attr.named.get("default")).get
      case _: Macro => ""
    }.mkString("")
}

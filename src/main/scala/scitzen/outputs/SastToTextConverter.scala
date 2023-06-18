package scitzen.outputs

import scitzen.sast.{BCommand, Block, Directive, Fenced, Inline, InlineText, ListItem, Paragraph, Parsed, Sast, Section, Slist, SpaceComment, Text}
import scitzen.compat.Logging.scribe
import scitzen.sast.DCommand.Lookup

case class SastToTextConverter(
    definitions: Map[String, String],
):

  def convert(b: Seq[Sast]): Seq[String] =
    b.flatMap {
      case Section(title, _, _) =>
        List(convertInline(title.inl))

      case Slist(children) => children.flatMap {
          case ListItem(_, Text(inl), None) =>
            List(convertInline(inl))
          case ListItem(_, text, Some(inner)) =>
            convertInline(text.inl) +: convert(List(inner))
        }

      case Block(command, attr, blockType) =>
        val filterBlock =
          command match
            case BCommand.If =>
              val res =
                definitions.get(attr.target) match
                  case Some(value) =>
                    attr.named.get("equals").forall(_ == value)
                  case None => false
              if attr.named.contains("not") then !res else res
            case _ => true

        if !filterBlock then Nil
        else
          blockType match
            case Paragraph(content)      => List(convertInline(content.inl), "")
            case Parsed(_, blockContent) => convert(blockContent)
            case Fenced(text)            => List(text)
            case SpaceComment(str)       => List(str)

      case mcro: Directive =>
        val attributes = mcro.attributes
        mcro.command match

          case Lookup =>
            definitions.get(attributes.target).orElse(attributes.named.get("default")) match
              case None =>
                scribe.error(s"could not resolve ${attributes.target}")
                Nil
              case Some(res) => List(res)

          case _ => Nil

    }

  def convertInline(inners: Seq[Inline]): String =
    inners.map {
      case InlineText(str) => str
      case Directive(Lookup, attr) =>
        definitions.get(attr.target).orElse(attr.named.get("default")).getOrElse("")
      case _: Directive => ""
    }.mkString("")

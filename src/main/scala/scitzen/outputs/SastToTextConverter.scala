package scitzen.outputs

import scitzen.generic.Sast
import scitzen.generic.Sast._
import scitzen.parser.{Inline, InlineText, Macro}


object SastToTextConverter {

  def convert(b: Seq[Sast]): Seq[String] = {
    convertSast(b)
  }

  def convertSast(b: Seq[Sast]): Seq[String] = {
    b.flatMap {
      case NoContent => Nil

      case Section(title, level, _) =>
        List(convertInline(title.inline))

      case Slist(children) => children.flatMap {
        case SlistItem(marker, Text(inl), NoContent) =>
          List(convertInline(inl))
        case SlistItem(marker, text, inner)                                =>
          convertInline(text.inline) +: convertSast(List(inner))
      }

      case SMacro(_) => Nil

      case SBlock(_, blockType) => blockType match {
        case Paragraph(content)      => List(convertInline(content.inline))
        case Parsed(_, blockContent) => convert(blockContent)
        case Fenced(text)            => List(text)
        case SpaceComment(_)         => Nil
      }


    }
  }


  def convertInline(inners: Seq[Inline]): String = inners.map {
    case InlineText(str)         => str
    case m: Macro                => ""
  }.mkString("")
}

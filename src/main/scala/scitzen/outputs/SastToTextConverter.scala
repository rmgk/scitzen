package scitzen.outputs

import scitzen.generic.Sast
import scitzen.generic.Sast._
import scitzen.parser.MacroCommand.Quote
import scitzen.parser.{Inline, InlineText, Macro}


object SastToTextConverter {

  def convert(b: Seq[Sast]): Seq[String] = {
    convertSast(b)
  }

  def convertSast(b: Seq[Sast]): Seq[String] = {
    b.flatMap {

      case Section(title, sc, _) =>
        convertInline(title.inline) +:
        convert(sc)

      case Slist(children) => children.flatMap {
        case SlistItem(marker, Seq(SBlock(_, Paragraph(Text(inl))))) =>
          List(convertInline(inl))
        case SlistItem(marker, inner)                                =>
          convertSast(inner)
      }

      case SMacro(_) => Nil

      case SBlock(_, Paragraph(content)) => List(convertInline(content.inline))


      case SBlock(_, ParsedBlock(_, blockContent)) => convert(blockContent)

      case SBlock(_, RawBlock(_, text)) => List(text)
    }
  }


  def convertInline(inners: Seq[Inline]): String = inners.map {
    case InlineText(str)         => str
    case Macro(Quote(q), inner2) => inner2.target
    case m: Macro                => ""
  }.mkString("")
}

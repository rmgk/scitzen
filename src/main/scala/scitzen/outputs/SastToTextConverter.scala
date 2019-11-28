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
        case SlistItem(marker, Seq(TLBlock(_, Paragraph(Text(inl))))) =>
          List(convertInline(inl))
        case SlistItem(marker, inner)                                 =>
          convertSast(inner)
      }

      case MacroBlock(_) => Nil

      case TLBlock(_, Paragraph(content)) => List(convertInline(content.inline))


      case TLBlock(_, ParsedBlock(_, blockContent)) => convert(blockContent)

      case TLBlock(_, RawBlock(_, text)) => List(text)
    }
  }


  def convertInline(inners: Seq[Inline]): String = inners.map {
    case InlineText(str)         => str
    case Macro(Quote(q), inner2) => inner2.target
    case m: Macro                => ""
  }.mkString("")
}

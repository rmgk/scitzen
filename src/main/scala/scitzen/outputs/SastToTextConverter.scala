package scitzen.outputs

import scitzen.generic.Sast
import scitzen.generic.Sast._
import scitzen.parser.MacroCommand.Quote
import scitzen.parser.{Inline, InlineText, Macro}


object SastToTextConverter {

  def convert(b: Seq[TLBlock]): Seq[String] = {
    convertSast(b.map(_.content))
  }

  def convertSast(b: Seq[Sast]): Seq[String] = {
    b.flatMap[String, Seq[String]] {

      case AttributeDef(a) => Nil



      case Section(title, sc) =>
        convertInline(title.inline) +:
        convert(sc)

      case Slist(children) => children.flatMap {
        case SlistItem(marker, Seq(Paragraph(Text(inl)))) =>
          List(convertInline(inl))
        case SlistItem(marker, inner) =>
          convertSast(inner)
      }

      case MacroBlock(mcro) => Nil

      case Paragraph(content) => List(convertInline(content.inline))


      case ParsedBlock(_, blockContent) => convert(blockContent)

      case RawBlock(_, text) => List(text)
    }
  }


  def convertInline(inners: Seq[Inline]): String = inners.map {
    case InlineText(str)        => str
    case Macro(Quote(q), inner2) => inner2.target
    case m: Macro               => ""
  }.mkString("")
}

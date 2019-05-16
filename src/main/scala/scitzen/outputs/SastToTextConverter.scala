package scitzen.outputs

import scitzen.generic.Sast
import scitzen.generic.Sast._
import scitzen.parser.{Inline, InlineQuote, InlineText, Macro}


object SastToTextConverter {


  def convert(b: Seq[Sast]): Seq[String] = {
    b.flatMap[String, Seq[String]] {

      case AttributeDef(a) => Nil

      case Text(inner) => List(convertInline(inner))


      case Section(title, sc) =>
        convertInline(title.inline) +:
        convert(sc)

      case Slist(children) => children.flatMap {
        case SlistItem(marker, Seq(Text(inl))) =>
          List(convertInline(inl))
        case SlistItem(marker, inner) =>
          convert(inner)
      }

      case MacroBlock(mcro) => Nil

      case ParsedBlock(_, blockContent) => convert(blockContent)

      case RawBlock(_, text) => List(text)


      case bwa: AttributedBlock =>
        convert(List(bwa.content))
    }
  }


  def convertInline(inners: Seq[Inline]): String = inners.map {
    case InlineText(str)        => str
    case InlineQuote(q, inner2) => inner2
    case m: Macro               => ""
  }.mkString("")
}

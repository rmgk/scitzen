package scitzen.generic

import scitzen.generic.Sast._
import scitzen.parser._


object SastAnalyzes {
  case class Target(id: String, resolution: Sast)
  case class AnalyzeResult(attributes: List[Attribute],
                           macros: List[Macro],
                           targets: List[Target],
                           blocks: List[TLBlock]) {
    def +(m: Macro): AnalyzeResult = copy(macros = m :: macros)
    def +(m: Attribute): AnalyzeResult = copy(attributes = m :: attributes)
    def +(m: Target): AnalyzeResult = copy(targets = m :: targets)
    def +(m: TLBlock): AnalyzeResult = copy(blocks = m :: blocks)
  }

  def analyze(input: Seq[TLBlock]) = {
    val AnalyzeResult(a, m, t, b) = analyzeAll(input, None, AnalyzeResult(Nil, Nil, Nil, Nil))
    AnalyzeResult(a.reverse, m.reverse, t.reverse, b.reverse)
  }

  def analyzeAll(inputs: Seq[TLBlock], scope: Option[Target], acc: AnalyzeResult): AnalyzeResult =
    inputs.foldLeft(acc)((cacc, sast) => analyzeR(sast, scope, cacc))


  def analyzeR(input: TLBlock, scope: Option[Target], acc: AnalyzeResult): AnalyzeResult = {
    input.content match {
      case rb :  RawBlock => acc + input
      case other => analyzeRSast(other, scope, acc)
    }
  }

  def analyzeAllSast(inputs: Seq[Sast], scope: Option[Target], acc: AnalyzeResult): AnalyzeResult =
    inputs.foldLeft(acc)((cacc, sast) => analyzeRSast(sast, scope, cacc))

  def analyzeRSast(input: Sast, scope: Option[Target], acc: AnalyzeResult): AnalyzeResult = input match {
    case Slist(children) => children.foldLeft(acc) { (cacc, sli) =>
      analyzeAllSast(sli.content, scope, cacc)
    }

    case sec @ Section(title, content)   =>
      val target = Target(title.str, sec)
      analyzeAll(content, Some(target), acc + target)
    case AttributeDef(attribute) => acc + attribute
    case MacroBlock(imacro)      =>
      val iacc =
        if (imacro.command == "label") {
          if (scope.isEmpty) scribe.error(s"unknown scope of label ${imacro.attributes.target}")
          acc + Target(imacro.attributes.target,
                       scope.get.resolution)
        }
        else acc
      iacc + imacro
    case Paragraph(content) => content.inline.foldLeft(acc) { (cacc, inline) =>
      inline.content match {
        case m: Macro              => cacc + m
        case InlineText(str)       => cacc
        case InlineQuote(q, inner) => cacc
      }
    }
    case ParsedBlock(delimiter, content) => analyzeAll(content, scope, acc)
    case RawBlock(_, _)                  => acc
  }


  def analyzeText(input: Text, scope: Option[Target], acc: AnalyzeResult): AnalyzeResult = {
    val inlines = input.inline.map(_.content)
    inlines.foldLeft(acc) { (cacc, inline) =>
      inline match {
        case m: Macro              => cacc + m
        case InlineText(str)       => cacc
        case InlineQuote(q, inner) => cacc
      }
    }
  }
}
package scitzen.generic

import scitzen.generic.Sast._
import scitzen.parser._


object SastAnalyzes {
  case class Target(id: String, resolution: Sast)
  case class AnalyzeResult(attributes: List[Attribute],
                           macros: List[Macro],
                           targets: List[Target]) {
    def +(m: Macro): AnalyzeResult = copy(macros = m :: macros)
    def +(m: Attribute): AnalyzeResult = copy(attributes = m :: attributes)
    def +(m: Target): AnalyzeResult = copy(targets = m :: targets)
  }

  def analyze(input: Seq[Sast]) = {
    val AnalyzeResult(a, m, t) = analyzeAll(input, None, AnalyzeResult(Nil, Nil, Nil))
    AnalyzeResult(a.reverse, m.reverse, t.reverse)
  }

  def analyzeAll(inputs: Seq[Sast], scope: Option[Target], acc: AnalyzeResult): AnalyzeResult =
    inputs.foldLeft(acc)((cacc, sast) => analyzeR(sast, scope, cacc))


  def analyzeR(input: Sast, scope: Option[Target], acc: AnalyzeResult): AnalyzeResult = input match {
    case Slist(children) => children.foldLeft(acc) { (cacc, sli) =>
      analyzeAll(sli.content, scope, acc)
    }
    case Text(inlines)   => inlines.foldLeft(acc) { (cacc, inline) =>
      inline match {
        case m: Macro              => cacc + m
        case InlineText(str)       => cacc
        case InlineQuote(q, inner) => cacc
      }
    }

    case sec @ Section(title, content)   =>
      val target = Target(title.str, sec)
      analyzeAll(content, Some(target), acc + target)
    case AttributeDef(attribute)         => acc + attribute
    case MacroBlock(imacro)              => {
      val iacc = if (imacro.command == "label") acc + Target(imacro.attributes.positional.head,
                                                             scope.get.resolution)
                 else acc
      iacc + imacro
    }
    case ParsedBlock(delimiter, content) => analyzeAll(content, scope, acc)
    case RawBlock(_, _)                  => acc
    case AttributedBlock(attr, content)  => analyzeR(content, scope, acc)
  }
}
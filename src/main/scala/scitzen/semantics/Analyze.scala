package scitzen.semantics

import scitzen.parser._
import scitzen.semantics.Sast._
import scitzen.semantics.SastAnalyzes.AnalyzeResult

case class Sdoc(sast: Seq[Sast]) {

  lazy val analyzeResult: AnalyzeResult = SastAnalyzes.analyze(sast)

  lazy val named: Map[String, String] = Attributes(analyzeResult.attributes).named

  lazy val language: String = named.getOrElse("lang", "").trim

  lazy val date    : Option[ScitzenDateTime] = named.get("revdate")
                                               .map(v => DateParsingHelper.parseDate(v.trim))
  lazy val modified: Option[ScitzenDateTime] = named.get("modified")
                                               .map(m => DateParsingHelper.parseDate(m.trim))

  lazy val title: Option[String] = sast.headOption.collect { case s : Section => s }.map(_.title.str)

  def targets = analyzeResult.targets

}

object Sdoc {
  def findSections(cont: Seq[Sast]): Seq[Section] = {
    cont.flatMap {
      case s: Section => List(s)
      case AttributedBlock(_, content) => findSections(List(content))
      case ParsedBlock(_, content) => findSections(content)
      case _ => Nil
    }
  }
}

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
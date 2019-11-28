package scitzen.generic

import scitzen.generic.Sast._
import scitzen.outputs.SastToScimConverter
import scitzen.parser.MacroCommand.{Label, Quote}
import scitzen.parser._

import scala.util.control.NonFatal

object SastAnalyzes {
  case class Target(id: String, resolution: Sast)
  case class AnalyzeResult(macros: List[Macro],
                           targets: List[Target],
                           rawBlocks: List[TLBlock],
                           sections: List[Section]) {
    def +(m: Macro): AnalyzeResult = copy(macros = m :: macros)
    def +(m: Target): AnalyzeResult = copy(targets = m :: targets)
    def +(m: TLBlock): AnalyzeResult = copy(rawBlocks = m :: rawBlocks)
    def +(m: Section): AnalyzeResult = copy(sections = m :: sections)
  }
}

class SastAnalyzes(macroReporter: Reporter) {
  import scitzen.generic.SastAnalyzes._

  def reportTarget(mcr: Macro): String =
    try {
      mcr.attributes.target
    }
    catch {
      case NonFatal(e) =>
        scribe.error(s"${new SastToScimConverter().macroToScim(mcr)} had no target" + macroReporter(
          mcr))
        throw e
    }



  def analyze(sdoc: Sdoc): AnalyzeResult = {
    val input = sdoc.blocks
    val AnalyzeResult(m, t, b, s) = analyzeAllSast(input, None, AnalyzeResult(Nil, Nil, Nil, Nil))
    AnalyzeResult(m.reverse, t.reverse, b.reverse, s.reverse)
  }

  def analyzeAll(inputs: Seq[TLBlock], scope: Option[Target], acc: AnalyzeResult): AnalyzeResult =
    inputs.foldLeft(acc)((cacc, sast) => analyzeR(sast, scope, cacc))


  def analyzeR(input: TLBlock, scope: Option[Target], acc: AnalyzeResult): AnalyzeResult = {
    input.content match {
      case rb :  RawBlock => acc + input
      case other => analyzeSBlockType(other, scope, acc)
    }
  }

  def analyzeAllSast(inputs: Seq[Sast], scope: Option[Target], acc: AnalyzeResult): AnalyzeResult =
    inputs.foldLeft(acc)((cacc, sast) => analyzeRSast(sast, scope, cacc))

  def analyzeRSast(input: Sast, scope: Option[Target], acc: AnalyzeResult): AnalyzeResult = input match {
    case Slist(children) => children.foldLeft(acc) { (cacc, sli) =>
      analyzeAllSast(sli.content, scope, cacc)
    }

    case sec @ Section(title, content, attributes) =>
      val target = Target(title.str, sec)
      analyzeAllSast(content, Some(target), acc + target + sec)
    case MacroBlock(imacro)                        =>
      val iacc =
        if (imacro.command == Label) {
          if (scope.isEmpty) scribe.error(s"unknown scope of label ${imacro.attributes.target}" + macroReporter(
            imacro))
            acc + Target(reportTarget(imacro), scope.fold(input)(_.resolution))
        }
        else acc
      iacc + imacro
    case TLBlock(attr, content) => analyzeSBlockType(content, scope, acc)
  }
    def analyzeSBlockType(input: SBlockType, scope: Option[Target], acc: AnalyzeResult): AnalyzeResult = input match {
    case Paragraph(content) => content.inline.foldLeft(acc) { (cacc, inline) =>
      inline match {
        case Macro(_ : Quote, inner) => cacc
        case m: Macro                => cacc + m
        case InlineText(str)         => cacc
      }
    }
    case ParsedBlock(delimiter, content) => analyzeAllSast(content, scope, acc)
    case RawBlock(_, _)                  => acc
  }


  def analyzeText(input: Text, scope: Option[Target], acc: AnalyzeResult): AnalyzeResult = {
    input.inline.foldLeft(acc) { (cacc, inline) =>
      inline match {
        case m: Macro                => cacc + m
        case InlineText(str)         => cacc
      }
    }
  }
}

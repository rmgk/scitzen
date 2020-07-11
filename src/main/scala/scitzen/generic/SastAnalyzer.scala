package scitzen.generic

import scitzen.parser.Sast._
import scitzen.parser._

object SastAnalyzer {
  case class AnalyzeResult(macros: List[Macro], rawBlocks: List[Block], sections: List[Section]) {
    def +(m: Macro): AnalyzeResult   = copy(macros = m :: macros)
    def +(m: Block): AnalyzeResult   = copy(rawBlocks = m :: rawBlocks)
    def +(m: Section): AnalyzeResult = copy(sections = m :: sections)
  }
}

class SastAnalyzer(val macroReporter: Reporter) {

  import scitzen.generic.SastAnalyzer._

  def analyze(input: Seq[Sast]): AnalyzeResult = {
    val AnalyzeResult(m, b, s) = analyzeAllSast(input, AnalyzeResult(Nil, Nil, Nil))
    AnalyzeResult(m.reverse, b.reverse, s.reverse)
  }

  def analyzeAllSast(inputs: Seq[Sast], acc: AnalyzeResult): AnalyzeResult =
    inputs.foldLeft(acc)((cacc, sast) => analyzeRSast(sast, cacc))

  def analyzeRSast(input: Sast, acc: AnalyzeResult): AnalyzeResult =
    input match {
      case Slist(children) => children.foldLeft(acc) { (cacc, sli) =>
          val tacc = analyzeText(sli.text, cacc)
          sli.content.fold(tacc)(analyzeRSast(_, tacc))
        }

      case sec @ Section(level, title, attributes) =>
        acc + sec

      case imacro @ Macro(_, _) =>
        acc + imacro

      case Block(attr, content) => analyzeSBlockType(content, acc)
    }
  def analyzeSBlockType(input: BlockType, acc: AnalyzeResult): AnalyzeResult =
    input match {
      case Paragraph(content)         => analyzeText(content, acc)
      case Parsed(delimiter, content) => analyzeAllSast(content, acc)
      case Fenced(_)                  => acc
      case SpaceComment(_)            => acc
    }

  def analyzeText(text: Text, acc: AnalyzeResult): AnalyzeResult = {
    text.inline.foldLeft(acc) { (cacc, inline) =>
      inline match {
        case m: Macro        => cacc + m
        case InlineText(str) => cacc
      }
    }

  }
}

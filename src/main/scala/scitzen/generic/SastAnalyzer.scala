package scitzen.generic

import scitzen.generic.Sast._
import scitzen.outputs.SastToScimConverter
import scitzen.parser._

import scala.util.control.NonFatal

object SastAnalyzer {
  case class AnalyzeResult(macros: List[Macro], rawBlocks: List[SBlock], sections: List[Section]) {
    def +(m: Macro): AnalyzeResult   = copy(macros = m :: macros)
    def +(m: SBlock): AnalyzeResult  = copy(rawBlocks = m :: rawBlocks)
    def +(m: Section): AnalyzeResult = copy(sections = m :: sections)
  }
}

class SastAnalyzer(val macroReporter: Reporter) {

  import scitzen.generic.SastAnalyzer._

  def reportTarget(mcr: Macro): String =
    try {
      mcr.attributes.target
    } catch {
      case NonFatal(e) =>
        scribe.error(s"${new SastToScimConverter().macroToScim(mcr)} had no target" + macroReporter(
          mcr
        ))
        throw e
    }

  def analyze(input: Seq[Sast]): AnalyzeResult = {
    val AnalyzeResult(m, b, s) = analyzeAllSast(input, AnalyzeResult(Nil, Nil, Nil))
    AnalyzeResult(m.reverse, b.reverse, s.reverse)
  }

  def analyzeAll(inputs: Seq[SBlock], acc: AnalyzeResult): AnalyzeResult =
    inputs.foldLeft(acc)((cacc, sast) => analyzeR(sast, cacc))

  def analyzeR(input: SBlock, acc: AnalyzeResult): AnalyzeResult = {
    input.content match {
      case rb: Fenced => acc + input
      case other      => analyzeSBlockType(other, acc)
    }
  }

  def analyzeAllSast(inputs: Seq[Sast], acc: AnalyzeResult): AnalyzeResult =
    inputs.foldLeft(acc)((cacc, sast) => analyzeRSast(sast, cacc))

  def analyzeRSast(input: Sast, acc: AnalyzeResult): AnalyzeResult =
    input match {
      case NoContent => acc

      case Slist(children) => children.foldLeft(acc) { (cacc, sli) =>
          val tacc = analyzeText(sli.text, cacc)
          analyzeRSast(sli.content, tacc)
        }

      case sec @ Section(level, title, attributes) =>
        acc + sec

      case SMacro(imacro) =>
        acc + imacro

      case SBlock(attr, content) => analyzeSBlockType(content, acc)
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

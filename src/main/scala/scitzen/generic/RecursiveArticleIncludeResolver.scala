package scitzen.generic

import better.files.File
import scitzen.parser.MacroCommand
import scitzen.parser.Sast.Macro

/* The only thing the includes are used for is to figure out reference targets.
 * This somehow should be much easier … */
object RecursiveArticleIncludeResolver {
  def recursiveIncludes(article: Article, project: Project, documentDirectory: DocumentDirectory): DocumentDirectory = {

    @scala.annotation.tailrec
    def rec(toCheck: List[File], knownIncludes: List[File]): List[File] = {
      val newKnown = toCheck reverse_::: knownIncludes
      val newIncludes =
        toCheck.iterator
          .flatMap(documentDirectory.byPath.get)
          .flatMap(_.includes)
          .filterNot(f => newKnown.contains(f)).toList
      if (newIncludes.isEmpty) newKnown
      else rec(newIncludes, newKnown)
    }

    val initialIncludes: List[File] =
      new SastAnalyzer(article.sourceDoc.reporter).analyze(article.content).macros.collect {
        case Macro(MacroCommand.Include, attributes) =>
          project.resolve(article.sourceDoc.file.parent, attributes.target)
      }.flatten

    val includes = rec(initialIncludes, Nil)
    val incd     = documentDirectory.documents.filter(d => includes.contains(d.file))
    DocumentDirectory(incd)

  }

  import scitzen.parser.Sast._
  import scitzen.parser._

  case class AnalyzeResult(macros: List[Macro]) {
    def +(m: Macro): AnalyzeResult = copy(macros = m :: macros)
  }

  class SastAnalyzer(val macroReporter: Reporter) {

    def analyze(input: Seq[Sast]): AnalyzeResult = {
      val AnalyzeResult(m) = analyzeAllSast(input, AnalyzeResult(Nil))
      AnalyzeResult(m.reverse)
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
          acc

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

}

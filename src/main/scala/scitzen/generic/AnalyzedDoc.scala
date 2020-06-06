package scitzen.generic

import better.files.File
import cats.implicits._
import scitzen.generic.Sast.Section
import scitzen.generic.SastAnalyzer.AnalyzeResult
import scitzen.outputs.SastToTextConverter
import scitzen.parser.MacroCommand.Def
import scitzen.parser.{Attributes, DateParsingHelper, Macro, Prov, ScitzenDateTime}

import scala.collection.immutable.ArraySeq
import scala.util.control.NonFatal

case class AnalyzedDoc(sast: List[Sast], analyzer: SastAnalyzer) {

  lazy val analyzeResult: AnalyzeResult = analyzer.analyze(sast)

  lazy val named: Map[String, String] = {
    val sectionattrs = analyzeResult.sections.flatMap(_.attributes.raw)
    val macroattrs   = analyzeResult.macros.filter(_.command == Def)
                                    .flatMap(m => m.attributes.raw)
    Attributes(macroattrs ++ sectionattrs, Prov()).named
  }

  lazy val language: Option[String] = named.get("language").map(_.trim)

  lazy val date: Option[ScitzenDateTime] = named.get("date")
                                                .map(v => DateParsingHelper.parseDate(v.trim))

  lazy val title: Option[String] = sast.headOption.collect { case s: Section => s }.map(_.title.str)

  lazy val words: List[String] = SastToTextConverter.convert(sast)
                                                    .flatMap(_.split("[^\\p{L}]+")).toList

  lazy val wordcount: Map[String, Int] =
    words.foldMap(s => Map(s.toLowerCase() -> 1))

  lazy val bigrams: Map[(String, String), Int] = {
    words.sliding(2, 1).toList.foldMap {
      case List(a, b) => Map((a.toLowerCase(), b.toLowerCase()) -> 1)
      case _          => Map()
    }
  }
}

object AnalyzedDoc {
  def apply(parsedDocument: ParsedDocument): AnalyzedDoc = {
    new AnalyzedDoc(parsedDocument.sast, new SastAnalyzer(parsedDocument.reporter))
  }
}

final case class ParsedDocument(file: File, content: String, sast: List[Sast]) {
  lazy val reporter: FileReporter = new FileReporter(file, content)
}

object ParsedDocument {
  def apply(file: File): ParsedDocument = {
    val content = file.contentAsString
    try {
      val sast = SastConverter().documentString(content, Prov(0, content.length))
      ParsedDocument(file, content, sast.toList)
    } catch {
      case NonFatal(e) =>
        scribe.error(s"error while parsing $file")
        throw e
    }
  }
}

trait Reporter {
  def apply(im: Macro): String = apply(im.attributes.prov)
  def apply(prov: Prov): String
}

final class FileReporter(file: File, content: String) extends Reporter {
  lazy val newLines: Seq[Int] = {
    def findNL(idx: Int, found: List[Int]): Array[Int] = {
      val res = content.indexOf('\n', idx + 1)
      if (res >= 0) findNL(res, res :: found)
      else found.toArray.reverse
    }

    ArraySeq.unsafeWrapArray(findNL(-1, Nil))
  }

  def indexToPosition(idx: Int): (Int, Int) = {
    val ip     = newLines.search(idx).insertionPoint
    val offset = if (ip == 0) 0 else newLines(ip - 1)
    (ip + 1, idx - offset)
  }

  override def apply(prov: Prov): String = {
    val pos = indexToPosition(prov.start)
    s" at »${File.currentWorkingDirectory.relativize(file)}:" +
    s"${pos._1}:${pos._2}«"
  }
}

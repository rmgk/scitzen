package scitzen.generic

import better.files.File
import cats.implicits._
import scitzen.parser.MacroCommand.Def
import scitzen.parser.Sast.{Macro, Section}
import scitzen.parser.{Attributes, DateParsingHelper, Parse, Prov, Sast, ScitzenDateTime}

import scala.collection.immutable.ArraySeq
import scala.util.control.NonFatal

case class Article(header: Section, content: List[Sast], sourceDoc: ParsedDocument) {

  lazy val language: Option[String] = header.attributes.named.get("language").map(_.trim)

  lazy val date: Option[ScitzenDateTime] = header.attributes.named.get("date")
    .map(v => DateParsingHelper.parseDate(v.trim))

  lazy val title: String = header.title.str

  lazy val named: Map[String, String] = header.attributes.named

  lazy val analyzed: AnalyzeResult = {
    new SastAnalyzer(sourceDoc.reporter).analyze(content)
  }
}

object Article {
  def notArticleHeader(sast: Sast): Boolean =
    sast match {
      case Section(title, "=", attributes) => false
      case other                           => true
    }

  def articles(parsedDocument: ParsedDocument, content: List[Sast]): List[Article] = {
    @scala.annotation.tailrec
    def rec(rem: List[Sast], acc: List[Article]): List[Article] = {
      rem.dropWhile(notArticleHeader) match {
        case (sec @ Section(title, "=", attributes)) :: rest =>
          val (cont, other) = rest.span(notArticleHeader)
          rec(other, Article(sec, cont, parsedDocument) :: acc)
        case other => acc.reverse
      }
    }
    rec(content, Nil)
  }
}

case class AnalyzedDoc(sast: List[Sast], analyzer: SastAnalyzer) {

  lazy val analyzeResult: AnalyzeResult = analyzer.analyze(sast)

  lazy val named: Map[String, String] = {
    val sectionattrs = analyzeResult.sections.flatMap(_.attributes.raw)
    val macroattrs = analyzeResult.macros.filter(_.command == Def)
      .flatMap(m => m.attributes.raw)
    Attributes(macroattrs ++ sectionattrs, Prov()).named
  }

  lazy val date: Option[ScitzenDateTime] = named.get("date")
    .map(v => DateParsingHelper.parseDate(v.trim))

  lazy val title: Option[String] = sast.headOption.collect { case s: Section => s }.map(_.title.str)
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
      val sast = Parse.documentUnwrap(content, Prov(0, content.length))
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

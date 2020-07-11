package scitzen.generic

import better.files.File
import scitzen.parser.Sast.Section
import scitzen.parser.{DateParsingHelper, Sast, ScitzenDateTime}

case class Article(header: Section, content: List[Sast], sourceDoc: Document, includes: Map[File, Document]) {

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

  def articles(document: Document): List[Article] = {
    @scala.annotation.tailrec
    def rec(rem: List[Sast], acc: List[Article]): List[Article] = {
      rem.dropWhile(notArticleHeader) match {
        case (sec @ Section(title, "=", attributes)) :: rest =>
          val (cont, other) = rest.span(notArticleHeader)
          rec(other, Article(sec, cont, document, Map.empty) :: acc)
        case other => acc
      }
    }
    rec(document.sast, Nil)
  }
}

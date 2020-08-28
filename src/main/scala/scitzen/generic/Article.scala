package scitzen.generic

import scitzen.parser.{DateParsingHelper, ScitzenDateTime}
import scitzen.sast.{Sast, Section}

case class Article(header: Section, content: List[Sast], sourceDoc: Document, includes: DocumentDirectory) {

  lazy val language: Option[String] = header.attributes.named.get("language").map(_.trim)

  lazy val date: Option[ScitzenDateTime] = header.attributes.named.get("date")
    .map(v => DateParsingHelper.parseDate(v.trim))

  lazy val title: String = header.title.str

  lazy val named: Map[String, String] = header.attributes.named
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
          rec(other, Article(sec, cont, document, DocumentDirectory(Nil)) :: acc)
        case other => acc
      }
    }
    rec(document.sast, Nil)
  }
}

package scitzen.generic

import scitzen.parser.TimeParsers
import scitzen.sast.{Sast, Section, ScitzenDateTime}

case class Article(header: Section, content: List[Sast], sourceDoc: Document) {

  lazy val language: Option[String] = header.attributes.named.get("language").map(_.trim)

  lazy val date: Option[ScitzenDateTime] = header.attributes.named.get("date")
    .map(v => TimeParsers.parseDate(v.trim))

  lazy val title: String = header.title.str

  lazy val named: Map[String, String] = header.attributes.named
}

object Article {
  def notArticleHeader(sast: Sast): Boolean =
    sast match {
      case Section(_, "=", _, prov) => false
      case _                        => true
    }

  def articles(document: Document): List[Article] = {
    @scala.annotation.tailrec
    def rec(rem: List[Sast], acc: List[Article]): List[Article] = {
      rem match {
        case (sec @ Section(_, "=", _, prov)) :: rest =>
          val (cont, other) = rest.span(notArticleHeader)
          rec(other, Article(sec, cont, document) :: acc)
        case _ => acc
      }
    }

    rec(document.sast.dropWhile(notArticleHeader), Nil)
  }
}

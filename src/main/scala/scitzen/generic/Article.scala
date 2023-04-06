package scitzen.generic

import scitzen.parser.TimeParsers
import scitzen.sast.{Sast, Section, ScitzenDateTime}

case class Article(header: Section, content: List[Sast], sourceDoc: Document):

  lazy val language: Option[String] = header.attributes.named.get("language").map(_.trim)

  lazy val date: Option[ScitzenDateTime] = header.attributes.named.get("date")
    .map(v => TimeParsers.parseDate(v.trim))

  lazy val title: String = header.title.plainString

  lazy val named: Map[String, String] = header.attributes.named

  lazy val filename: Option[String] = named.get("filename")

  def sast: List[Sast] = header :: content

object Article:
  def notHeader(sast: Sast): Boolean =
    sast match
      case Section(_, "=", _) => false
      case _                  => true

  def articles(document: Document): List[Article] =
    @scala.annotation.tailrec
    def rec(rem: List[Sast], acc: List[Article]): List[Article] =
      rem match
        case (sec @ Section(_, "=", _)) :: rest =>
          val (cont, other) = rest.span(notHeader)
          rec(other, Article(sec, cont, document) :: acc)
        case _ => acc

    rec(document.sast.dropWhile(notHeader), Nil)

object ArticleItem:
  def notHeader(sast: Sast): Boolean =
    sast match
      case Section(_, "==", _) => false
      case _                   => true

  def items(document: Document): List[Article] =
    @scala.annotation.tailrec
    def rec(rem: List[Sast], acc: List[Article]): List[Article] =
      rem match
        case (sec @ Section(_, "==", _)) :: rest =>
          val (cont, other) = rest.span(a => notHeader(a) && Article.notHeader(a))
          rec(other.dropWhile(notHeader), Article(sec, cont, document) :: acc)
        case Nil           => acc
        case other :: rest => throw IllegalStateException(s"unexpected sast when looking for item: $other")

    rec(document.sast.dropWhile(notHeader), Nil)

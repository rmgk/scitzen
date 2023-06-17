package scitzen.generic

import scitzen.contexts.SastContext
import scitzen.sast.{Sast, Section}

case class Article(content: List[Sast], sourceDoc: Document, context: SastContext[Unit], prior: List[Sast]):
  def titled: Option[Section] = content match
    case (h @ Section(_, _ @ ("=" | "=="), _)) :: rest => Some(h)
    case other                                         => None

  lazy val settings: Map[String, String] = titled match
    case Some(sect) => sect.attributes.named
    case None => Map.empty

case class TitledArticle(header: Section, article: Article):
  def body: List[Sast]           = article.content.tail
  def named: Map[String, String] = header.attributes.named
  def title                      = header.title
  def full = header.prefix == "="
  def date = header.date

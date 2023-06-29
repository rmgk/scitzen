package scitzen.generic

import scitzen.contexts.SastContext
import scitzen.sast.{Sast, Section}

case class Article(sast: List[Sast], doc: Document, context: SastContext[Unit], prior: List[Sast]):
  def titled: Option[Section] = sast match
    case (h @ Section(_, _ @("=" | "=="), _)) :: rest => Some(h)
    case other                                        => None


case class TitledArticle(header: Section, article: Article)

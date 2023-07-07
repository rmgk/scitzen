package scitzen.generic

import scitzen.compat.Logging
import scitzen.contexts.SastContext
import scitzen.sast.{Sast, Section}

case class Article(ref: ArticleRef, sast: List[Sast], doc: Document, context: SastContext[Unit], prior: List[Sast]):
  def titled: Option[Section] = sast match
    case (h @ Section(_, _ @("=" | "=="), _)) :: rest => Some(h)
    case other                                        => None

case class TitledArticle(header: Section, article: Article, flags: Flags)

class ArticleRef(val document: Document)

case class Flags(
    html: Boolean,
    tex: Boolean,
    hardwrap: Boolean,
    justify: Boolean,
    `section numbers`: Boolean,
    hidden: Boolean
):

  def apply(updates: Iterator[String]) =
    updates.foldLeft(Flags.default): (curr, flag) =>
      flag match
        case "+html"            => copy(html = true)
        case "-html"            => copy(html = false)
        case "+tex"             => copy(tex = true)
        case "-tex"             => copy(tex = false)
        case "+justify"         => copy(justify = true)
        case "-justify"         => copy(justify = false)
        case "+hardwrap"        => copy(justify = true)
        case "-hardwrap"        => copy(justify = false)
        case "+section numbers" => copy(`section numbers` = true)
        case "-section numbers" => copy(`section numbers` = false)
        case "+hidden"          => copy(hidden = true)
        case "-hidden"          => copy(hidden = false)
        case other: String =>
          Logging.cli.warn(s"unknown flag", other)
          this
object Flags:
  def default: Flags =
    Flags(html = true, tex = false, hardwrap = false, justify = true, `section numbers` = true, hidden = false)

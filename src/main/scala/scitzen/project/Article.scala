package scitzen.project

import scitzen.compat.Logging
import scitzen.contexts.SastContext
import scitzen.sast.Fusion.Atoms
import scitzen.sast.{Sast, Section}

case class Article(ref: ArticleRef, sast: List[Sast], doc: Document, context: SastContext[Unit], atoms: Atoms):
  def titled: Option[Section] = sast match
    case (h @ Section(_, _ @("=" | "=="), _, _)) :: rest => Some(h)
    case other                                           => None

case class TitledArticle(header: Section, article: Article, flags: Flags)

class ArticleRef(val document: Document)

case class Flags(
    html: Boolean,
    tex: Boolean,
    hardwrap: Boolean,
    justify: Boolean,
    `section numbers`: Boolean,
    hidden: Boolean,
    notes: Boolean,
):

  def apply(updates: Iterator[String]) =
    updates.foldLeft(this): (curr, flag) =>
      flag match
        case "+html"            => curr.copy(html = true)
        case "-html"            => curr.copy(html = false)
        case "+tex"             => curr.copy(tex = true)
        case "-tex"             => curr.copy(tex = false)
        case "+justify"         => curr.copy(justify = true)
        case "-justify"         => curr.copy(justify = false)
        case "+hardwrap"        => curr.copy(hardwrap = true)
        case "-hardwrap"        => curr.copy(hardwrap = false)
        case "+section numbers" => curr.copy(`section numbers` = true)
        case "-section numbers" => curr.copy(`section numbers` = false)
        case "+hidden"          => curr.copy(hidden = true)
        case "-hidden"          => curr.copy(hidden = false)
        case "+notes"           => curr.copy(notes = true)
        case "-notes"           => curr.copy(notes = false)
        case other: String =>
          Logging.cli.warn(s"unknown flag", other)
          this
object Flags:
  def default: Flags =
    Flags(
      html = true,
      tex = false,
      hardwrap = true,
      justify = true,
      `section numbers` = true,
      hidden = false,
      notes = true
    )

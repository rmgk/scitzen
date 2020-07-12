package scitzen.outputs

import org.jsoup.Jsoup
import scalatags.Text.Frag
import scalatags.Text.all.SeqFrag
import scalatags.Text.attrs.{`for`, `type`, charset, cls, content, hidden, href, id, lang, name, rel, title}
import scalatags.Text.implicits.{Tag, raw, stringAttr}
import scalatags.Text.tags.{body, head, html, input, label, link, meta}
import scalatags.Text.tags2.{aside, main, nav}

object HtmlPages {
  def apply(cssPath: String): HtmlPages = new HtmlPages(cssPath)
}

class HtmlPages(cssPath: String) {

  val tHead: Tag = {
    head(
      title := "Scitzen",
      link(href := cssPath, rel := "stylesheet", `type` := "text/css"),
      meta(name := "viewport", content := "width=device-width, initial-scale=1, user-scalable=yes, minimal-ui"),
      meta(charset := "UTF-8")
    )
  }

  def htmlDocument(tag: Tag): String =
    "<!DOCTYPE html>\n" + Jsoup.parse(tag.render).outerHtml()

  val sidebarContainer: Tag =
    aside(
      input(`type` := "checkbox", id := "sidebar-switch", hidden),
      label(
        `for` := "sidebar-switch",
        hidden,
        raw("""
              |<svg viewBox="0 0 48 48" width="48px" height="48px">
              |  <g stroke="currentColor" stroke-width="4" stroke-linecap="round">
              |  <path d="M 6 12 H 42" />
              |  <path d="M 6 24 H 42" />
              |  <path d="M 6 36 H 42" />
              |  </g>
              |</svg>
              |""".stripMargin)
      )
    )

  def wrapContentHtml(content: Seq[Frag], bodyClass: String, sidebar: Option[Frag], language: Option[String] = None): String = {

    htmlDocument(html(tHead)(body(
      cls := bodyClass,
      sidebar.map(s => sidebarContainer(nav(s))).toSeq,
      main(content)(language.map(lang := _).toSeq: _*)
    )))
  }

}

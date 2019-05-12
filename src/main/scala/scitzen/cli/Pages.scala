package scitzen.cli

import scalatags.Text.Frag
import scalatags.Text.all.{SeqFrag, frag}
import scalatags.Text.attrs.{`for`, `type`, charset, cls, content, href, id, lang, name, rel, title}
import scalatags.Text.implicits.{Tag, raw, stringAttr}
import scalatags.Text.tags.{body, div, head, html, input, label, link, meta}
import scalatags.Text.tags2.{main, nav}

object Pages {
  def apply(relative: String = ""): Pages = new Pages(relative)
}

class Pages(path_css: String) {

  val tHead = {
    head(
      title := "Scitzen",
      link(href := path_css, rel := "stylesheet", `type` := "text/css"),
      meta(name := "viewport", content := "width=device-width, initial-scale=1, user-scalable=yes, minimal-ui"),
      meta(charset := "UTF-8")
    )
  }


  def htmlDocument(tag: Tag): String = "<!DOCTYPE html>" + tag.render

  val sidebarContainer: Tag =
    div(
      cls := "sidebar",
      input(`type` := "checkbox", id := "sidebar-switch"),
      label(`for` := "sidebar-switch", raw("""
              |<svg viewBox="0 0 48 48">
              |  <g stroke="black" stroke-width="4" stroke-linecap="round">
              |  <path d="M 6 12 H 42" />
              |  <path d="M 6 24 H 42" />
              |  <path d="M 6 36 H 42" />
              |  </g>
              |</svg>
              |""".stripMargin)))

  def wrapContentHtml(content: Seq[Frag], bodyClass: String, sidebar: Option[Frag], language: String = ""): String = {


    htmlDocument(html(tHead)(body(
      cls := bodyClass,
      sidebar.map(s => sidebarContainer(nav(s))).toSeq,
      main(content)(if (language.nonEmpty) lang := language else frag()),
      )))
  }


}

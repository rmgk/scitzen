package scitzen.outputs

import scalatags.Text.{Frag, Tag, RawFrag}
import scalatags.Text.all.SeqFrag
import scalatags.Text.attrs.{`for`, `type`, charset, cls, content, hidden, href, id, lang, name, rel}
import scalatags.Text.implicits.{raw, stringAttr, stringFrag}
import scalatags.Text.tags.{body, head, html, input, label, link, meta}
import scalatags.Text.tags2.{aside, main, nav, title}

object HtmlPages:
  def apply(cssPath: String): HtmlPages = new HtmlPages(cssPath)

class HtmlPages(cssPath: String):

  def tHead(titled: String): Tag =
    head(
      title(titled),
      link(href    := cssPath, rel        := "stylesheet", `type` := "text/css"),
      meta(name    := "viewport", content := "width=device-width, initial-scale=1, user-scalable=yes, minimal-ui"),
      meta(charset := "UTF-8")
    )

  def htmlDocument(tag: Tag): String =
    "<!DOCTYPE html>\n" + tag.render

  def featherIcon(cls: String, path: String): RawFrag =
    raw(s"""<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="$cls"><path d="$path" /></svg>""")
  val sidebarContainer: Tag =
    aside(
      input(`type` := "checkbox", id := "sidebar-switch", hidden),
      label(
        `for` := "sidebar-switch",
        hidden,
        featherIcon("menu", "M3 12 H21 M3 6 H21 M3 18 H21"),
        featherIcon("close", "M18 6 L6 18 M6 6 L18 18")
      )
    )

  def wrapContentHtml(
      content: Seq[Frag],
      bodyClass: String,
      sidebar: Option[Frag],
      titled: String,
      language: Option[String] = None
  ): String =
    htmlDocument(html(
      // we define a global language as scitzen controls are kinda all english, but also to enable features such as hyphenation even if no language is defined. This will produce incorrect hyphenation, but thats guesswork anyways, so may be OK.
      lang := "en",
      tHead(titled)
    )(body(
      cls := bodyClass,
      sidebar.map(s => sidebarContainer(nav(s))).toSeq,
      main(content)(language.map(lang := _).toSeq*)
    )))

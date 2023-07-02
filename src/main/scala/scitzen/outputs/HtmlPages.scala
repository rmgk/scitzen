package scitzen.outputs

import scalatags.Text.{Frag, RawFrag, Tag}
import scalatags.Text.all.{OptionNode, SeqFrag}
import scalatags.Text.attrs.{`for`, `type`, charset, cls, content, hidden, href, id, lang, name, rel}
import scalatags.Text.implicits.{raw, stringAttr}
import scalatags.Text.tags.{body, head, html, input, label, link, meta}
import scalatags.Text.tags2.{aside, main, nav, title}
import scitzen.outputs.HtmlPages.svgContainer

object HtmlPages:

  val svgContainer =
    raw(s"""<svg display="none">${featherSymbol(
        "external-link",
        """<path d="M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6"></path><polyline points="15 3 21 3 21 9"></polyline><line x1="10" y1="14" x2="21" y2="3"></line>"""
      )}</svg>""")

  def featherSymbol(id: String, path: String) =
    s"""<symbol id="$id" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">$path</symbol>"""

  def featherIcon(cls: String, path: String): RawFrag =
    raw(
      s"""<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="$cls">$path</svg>"""
    )

  val iconMenu = featherIcon("menu", """<path d="M3 12 H21 M3 6 H21 M3 18 H21" />""")

  val iconClose = featherIcon("close", """<path d="M18 6 L6 18 M6 6 L18 18" />""")
  val iconExternalLink = raw(s"""<svg class="icon"><use href="#external-link"></use></svg>""")

class HtmlPages(cssPath: String):

  val tHead: Tag =
    head(
      meta(charset := "UTF-8"),
      meta(name    := "viewport", content := "width=device-width, initial-scale=1, user-scalable=yes, minimal-ui"),
      link(href    := cssPath, rel        := "stylesheet", `type` := "text/css"),
    )

  def htmlDocument(tag: Tag): String =
    "<!DOCTYPE html>\n" + tag.render

  val sidebarContainer: Tag =
    aside(
      input(`type` := "checkbox", id := "sidebar-switch", hidden),
      label(
        `for` := "sidebar-switch",
        hidden,
        HtmlPages.iconMenu,
        HtmlPages.iconClose,
      )
    )

  def wrapContentHtml(
      content: Seq[Frag],
      bodyClass: String,
      mainClass: Option[String],
      sidebar: Option[Frag],
      titled: Frag,
      language: Option[String] = None
  ): String =
    htmlDocument(html(
      // we define a global language as scitzen controls are kinda all english, but also to enable features such as hyphenation even if no language is defined. This will produce incorrect hyphenation, but thats guesswork anyways, so may be OK.
      lang := "en",
      tHead(title(titled)),
    )(body(
      cls := bodyClass,
      sidebar.map(s => sidebarContainer(nav(s))).toSeq,
      main(content, mainClass.map(cls := _), language.map(lang := _)),
      svgContainer,
    )))

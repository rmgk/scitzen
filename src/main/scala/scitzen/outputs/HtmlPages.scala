package scitzen.outputs

import scalatags.Text.{Frag, RawFrag}
import scalatags.Text.implicits.{raw}
import scitzen.html.sag.{Recipe, Sag, SagContext, SagContentWriter}
import scitzen.outputs.HtmlPages.svgContainer

import java.nio.charset.StandardCharsets

object HtmlPages:

  val svgContainer =
    s"""<svg display="none">${featherSymbol(
        "external-link",
        """<path d="M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6"></path><polyline points="15 3 21 3 21 9"></polyline><line x1="10" y1="14" x2="21" y2="3"></line>"""
      )}</svg>"""

  def featherSymbol(id: String, path: String) =
    s"""<symbol id="$id" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">$path</symbol>"""

  def featherIcon(cls: String, path: String): RawFrag =
    raw(
      s"""<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="$cls">$path</svg>"""
    )

  val iconMenu = featherIcon("menu", """<path d="M3 12 H21 M3 6 H21 M3 18 H21" />""")

  val iconClose        = featherIcon("close", """<path d="M18 6 L6 18 M6 6 L18 18" />""")
  val iconExternalLink = raw(s"""<svg class="icon"><use href="#external-link"></use></svg>""")

class HtmlPages(cssPath: String):

  def tHead(title: Recipe): Recipe =
    Sag.head(
      Sag.meta(charset = "UTF-8"),
      Sag.meta(name = "viewport", content = "width=device-width, initial-scale=1, user-scalable=yes, minimal-ui"),
      Sag.link(href = cssPath, rel = "stylesheet", `type` = "text/css"),
      title
    )

  def sidebarContainer(child: Recipe): Recipe =
    Sag.aside(
      Sag.input(`type` = "checkbox", id = "sidebar-switch", hidden = true),
      Sag.label(
        `for` = "sidebar-switch",
        hidden = true,
        HtmlPages.iconMenu,
        HtmlPages.iconClose,
      ),
      child
    )

  def wrapContentHtml(
      content: Recipe,
      bodyClass: String,
      mainClass: Option[String],
      sidebar: Option[Frag],
      titled: Frag,
      language: Option[String] = None
  ): String =
    val sctx = new SagContext()
    Sag.Concat(
      Sag.`!DOCTYPE html`(),
      Sag.html(
        // we define a global language as scitzen controls are kinda all english, but also to enable features such as hyphenation even if no language is defined. This will produce incorrect hyphenation, but thats guesswork anyways, so may be OK.
        lang = "en",
        tHead(Sag.title(Sag.Raw(titled.render))),
        Sag.body(
          `class` = bodyClass,
          sidebar.map(s => sidebarContainer(Sag.nav(s))).toSeq,
          Sag.main(content, `class` = mainClass, lang = language),
          Sag.Raw(svgContainer),
        )
      )
    ).runInContext(sctx)
    sctx.resultString

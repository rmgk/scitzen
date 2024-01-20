package scitzen.outputs

import de.rmgk.delay.Sync
import scitzen.contexts.ConversionContext
import scitzen.html.sag
import scitzen.html.sag.{Recipe, Sag, SagContentWriter, SagContext}
import scitzen.outputs.HtmlPages.svgContainer
import scitzen.sast.Section

import java.nio.charset.StandardCharsets
import scala.math.Ordering.Implicits.infixOrderingOps

object HtmlPages:

  val svgContainer =
    Sag.Raw(s"""<svg display="none">${featherSymbol(
        "external-link",
        """<path d="M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6"></path><polyline points="15 3 21 3 21 9"></polyline><line x1="10" y1="14" x2="21" y2="3"></line>"""
      )}</svg>""")

  def featherSymbol(id: String, path: String) =
    s"""<symbol id="$id" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">$path</symbol>"""

  def featherIcon(cls: String, path: String): Recipe =
    Sag.Raw(
      s"""<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="$cls">$path</svg>"""
    )

  val iconMenu = featherIcon("menu", """<path d="M3 12 H21 M3 6 H21 M3 18 H21" />""")

  val iconClose        = featherIcon("close", """<path d="M18 6 L6 18 M6 6 L18 18" />""")
  val iconExternalLink = s"""<svg class="icon"><use href="#external-link"></use></svg>"""

  val maxdepth     = 2
  val startsection = List("==", "#")

  def tableOfContents(docsections: List[Section], converter: SastToHtmlConverter): Option[Recipe] =

    def recurse(remaining: List[Section], depth: Int): Option[Recipe] =
      remaining match
        case Nil => None
        case (head: Section) :: rest =>
          val (sub, other) = rest.span(e => e > head)
          val subtags = if depth < maxdepth then recurse(sub, depth + 1)
          else None
          val res     = converter.convertInlineSeq(ConversionContext(()), head.titleText.inl)
          val thistag = Sag.li(Sag.a(href = s"#${head.ref}", res.data), subtags.map(Sag.ol(_)).toList)
          val nexttag = recurse(other, depth)
          Some:
            Sync[SagContext]:
              thistag.run
              nexttag.foreach(_.run)

    recurse(docsections.dropWhile(s => !startsection.contains(s.prefix)), 1).map(Sag.ol(_))

class HtmlPages(cssPath: String):

  inline def tHead(inline title: Recipe): Recipe =
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
      bodyClass: Option[String],
      mainClass: Option[String],
      sidebar: Option[Recipe],
      titled: Recipe,
      language: Option[String] = None
  ): String =
    val sctx = new SagContext()
    Sag.`!DOCTYPE html`().runInContext(sctx)
    Sag.html(
      // we define a global language as scitzen controls are kinda all english, but also to enable features such as hyphenation even if no language is defined. This will produce incorrect hyphenation, but thats guesswork anyways, so may be OK.
      lang = language,
      tHead(Sag.title(titled)),
      Sag.body(
        `class` = bodyClass,
        sidebar.map(s => sidebarContainer(Sag.nav(s))).toSeq,
        Sag.main(content, `class` = mainClass),
        svgContainer,
      )
    ).runInContext(sctx)
    sctx.resultString

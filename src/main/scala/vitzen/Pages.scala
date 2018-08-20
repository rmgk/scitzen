package vitzen

import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

import scalatags.Text.all.{frag, raw}
import scalatags.Text.attrs.{`type`, cls, content, href, rel, src, title, name => attrname}
import scalatags.Text.implicits.{Tag, stringAttr, stringFrag}
import scalatags.Text.tags.{body, h1, head, header, html, link, meta, script, span, a => anchor}
import scalatags.Text.tags2.{article, main, section}
import scalatags.Text.{Frag, Modifier, TypedTag}

object Pages {
  def apply(relative: String = ""): Pages = new Pages(relative)
}

class Pages(val relative: String) {


  val path_css: String = s"${relative}vitzen.css"
  val path_posts: String = s"${relative}posts"
  val path_highligh_js: String = s"${relative}highlight.js"


  private def tHead = {
    head(
      title := "Vitzen",
      link(href := path_css, rel := "stylesheet", `type` := "text/css"),
      script(src := path_highligh_js),
      script(raw("hljs.initHighlightingOnLoad();")),
      meta(attrname := "viewport", content := "width=device-width, initial-scale=1, user-scalable=yes, minimal-ui"))
  }

  private def tBody(content: Frag) = {
    body(main(content))
  }

  private def tMeta(post: Post) = {
    frag(timeSpan(post),
         frag(post.modified
              .map(mt => span(cls := "time", s" Modified ${mt.toLocalDate} ${mt.toLocalTime} "))
              .toList: _*),
         categoriesSpan(post)
    )
  }

  private def categoriesSpan(post: Post) = {
    span(cls := "category")((post.categories() ++ post.people()).map(c => stringFrag(s" $c ")): _*)
  }

  private def timeSpan(post: Post) = {
    //need time formatter, because to string removes seconds if all zero
    span(cls := "time", s" ${post.date.toLocalDate} ${post.date.toLocalTime.format(DateTimeFormatter.ISO_LOCAL_TIME)}")
  }
  private def tSingle(title: String, meta: Frag, content: Frag) = {
    article(cls := "fullpost",
            header(h1(raw(title)),
                   meta
            ),
            content
    )
  }

  private def tSection(posts: List[Post]): Frag = {
    val byYear: Map[Int, List[Post]] = posts.groupBy(_.date.getYear)
    frag(byYear.keys.toList.sorted(Ordering[Int].reverse).map { year =>
      val dhs = byYear.apply(year).sortBy(_.date.toEpochSecond(ZoneOffset.UTC))(Ordering[Long].reverse)
      section(cls := "year",
              h1(dhs.head.date.format(DateTimeFormatter.ofPattern("YYYY"))),
              frag(dhs.map { post =>
                article(timeSpan(post),
                        anchor(href := s"$path_posts/${post.targetPath()}", raw(post.title)),
                        categoriesSpan(post)
                )
              }: _*)
      )
    }: _*)
  }

  def makeHtml(stuff: Modifier*): TypedTag[String] =
    html(tHead)(stuff: _*)


  def htmlDocument(tag: Tag): String = "<!DOCTYPE html>" + tag.render


  def makePostHtml(post: Post): String = {
    htmlDocument(makeHtml(tBody(tSingle(post.title, tMeta(post), raw(post.content)))))
  }

  def makeIndexOf(posts: List[Post]): String = {
    htmlDocument(makeHtml(tBody(tSection(posts))))
  }


}

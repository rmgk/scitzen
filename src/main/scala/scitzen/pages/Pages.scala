package scitzen.pages

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneOffset}

import scalatags.Text.all.{frag, raw}
import scalatags.Text.attrs.{`type`, charset, cls, content, href, rel, src, title, name => attrname}
import scalatags.Text.implicits.{Tag, stringAttr, stringFrag}
import scalatags.Text.tags.{a, body, h1, head, header, html, link, meta, p, script, span}
import scalatags.Text.tags2.{article, main, section}
import scalatags.Text.{Frag, Modifier, TypedTag}
import scitzen.converter.{DateParsingHelper, Post}

import scala.util.Try

object Pages {
  def apply(relative: String = ""): Pages = new Pages(relative)
}

class Pages(val relative: String) {


  val path_css        : String = s"${relative}scitzen.css"
  val path_posts      : String = s"${relative}posts"
  val path_highligh_js: String = s"${relative}highlight.js"


  private def tHead = {
    head(
      title := "Scitzen",
      link(href := path_css, rel := "stylesheet", `type` := "text/css"),
      script(src := path_highligh_js),
      script(raw("hljs.initHighlightingOnLoad();")),
      meta(attrname := "viewport", content := "width=device-width, initial-scale=1, user-scalable=yes, minimal-ui"),
      meta(charset := "UTF-8")
    )
  }

  private def tBody(content: Frag) = {
    body(main(content))
  }

  private def tMeta(post: Post) = {
    p(cls := "metadata",
      timeFull(post.date),
      frag(post.modified.map(timeFull).toList: _*),
      categoriesSpan(post),
      frag(post.folder().map(f => span(cls := "category")(stringFrag(s" in $f"))).toList: _*)
    )
  }

  private def categoriesSpan(post: Post) = {
    span(cls := "category")((post.categories() ++ post.people()).map(c => stringFrag(s" $c ")): _*)
  }

  private def timeFull(date: LocalDateTime) = {
    //need time formatter, because to string removes seconds if all zero
    span(cls := "time",
         s" ${date.toLocalDate} ${Try {
           date.toLocalTime.format(DateTimeFormatter.ISO_LOCAL_TIME)}.getOrElse("")} ")
  }

  private def timeShort(date: LocalDateTime) = {
    //need time formatter, because to string removes seconds if all zero
    span(cls := "time",
         stringFrag({
           date.format(DateParsingHelper.monthDayTime)}))
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
              h1(dhs.head.date.format(DateTimeFormatter.ofPattern("yyyy"))),
              frag(dhs.map { post =>
                article(timeShort(post.date),
                        a(href := s"$path_posts/${post.targetPath()}", raw(post.title)),
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

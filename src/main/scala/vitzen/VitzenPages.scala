package vitzen

import java.nio.file.Path
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

import scalatags.Text.all.{frag, raw}
import scalatags.Text.attrs.{`type`, cls, content, href, id, rel, title, name => attrname}
import scalatags.Text.implicits.{Tag, stringAttr, stringFrag}
import scalatags.Text.tags.{div, h1, h2, head, header, html, link, meta, span, a => anchor}
import scalatags.Text.tags2.{article, main, section}
import scalatags.Text.{Frag, Modifier, TypedTag}


class VitzenPages(asciiData: AsciiData, contentPath: Path) {


  val path_css: String = "vitzen.css"


  private def tHead = {
    head(
      title := "Vitzen",
       link(href := path_css, rel := "stylesheet", `type` := "text/css"),
      meta(attrname := "viewport", content := "width=device-width, initial-scale=1, user-scalable=yes, minimal-ui"))
  }

  private def tBody(content: Frag) = {
    div(cls := "container", id := "mobile-panel",
        main(cls := "main", id := "main",
             div(cls := "content-wrapper",
                 div(cls := "content", id := "content",
                     content
                 )
             )
        )
    )
  }

  private def tMeta(post: Post) = {
    div(cls := "post-meta",
        span(cls := "post-time", s" ${post.date.toLocalDate} ${post.date.toLocalTime}"),
        frag(post.modified.map(mt => span(cls := "post-time", s" Modified ${mt.toLocalDate} ${mt.toLocalTime} ")).toList: _*),
        span(cls := "post-category")(post.categories().map(c => stringFrag(s" $c ")): _*)
    )
  }

  private def tSingle(title: String, meta: Frag, content: Frag) = {
    article(cls := "post",
            header(cls := "post-header",
                   h1(cls := "post-title",
                      raw(title)
                   ),
                   meta
            ),
            div(cls := "post-content",
                content
            )
    )
  }

  private def tSection(posts: List[Post]): Frag = {
    val byYear: Map[Int, List[Post]] = posts.groupBy(_.date.getYear)
    frag(byYear.keys.toList.sorted(Ordering[Int].reverse).map { year =>
      val dhs = byYear.apply(year).sortBy(_.date.toEpochSecond(ZoneOffset.UTC))(Ordering[Long].reverse)
      section(id := "archive", cls := "archive",
              div(cls := "collection-title",
                  h2(cls := "archive-year", dhs.head.date.format(DateTimeFormatter.ofPattern("YYYY")))
              ),
              section(id := "posts", cls := "posts")(dhs.map { post =>
                article(cls := "archive-post",
                        tMeta(post),
                        span(cls := "archive-post-title",
                             anchor(cls := "archive-post-link", href := s"posts/${post.targetPath()}", raw(post.title))
                        )
                )
              }: _*)
      )
    }: _*)
  }

  def makeHtml(stuff: Modifier*): TypedTag[String] =
    html(tHead)(stuff: _*)


  def htmlResponse(tag: Tag): String = "<!DOCTYPE html>" + tag.render


  def getContent(post: Post): String = {
    val res = htmlResponse(makeHtml(tBody(tSingle(post.title, tMeta(post),
                                                  raw(post.content)))))
    res
  }

  def archive(): String = {
    val docs: List[Post] = asciiData.allPosts
    htmlResponse(makeHtml(tBody(tSection(docs))))
  }


}

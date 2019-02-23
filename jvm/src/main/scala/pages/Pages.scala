package pages

import scalatags.Text.all.{frag, raw}
import scalatags.Text.attrs.{`type`, charset, cls, content, href, rel, title, name => attrname}
import scalatags.Text.implicits.{Tag, stringAttr, stringFrag}
import scalatags.Text.tags.{a, body, h1, head, header, html, link, meta, p, span}
import scalatags.Text.tags2.{article, main, section}
import scalatags.Text.{Frag, Modifier, TypedTag}
import scitzen.converter.{HtmlConverter, Post}
import scitzen.parser.ScitzenDateTime

object Pages {
  def apply(relative: String = ""): Pages = new Pages(relative)
}

class Pages(val relative: String) {


  val path_css        : String = s"${relative}scitzen.css"
  val path_posts      : String = s"${relative}posts"


  private def tHead = {
    head(
      title := "Scitzen",
      link(href := path_css, rel := "stylesheet", `type` := "text/css"),
      meta(attrname := "viewport", content := "width=device-width, initial-scale=1, user-scalable=yes, minimal-ui"),
      meta(charset := "UTF-8")
    )
  }

  private def tBody(content: Frag) = {
    body(main(content))
  }

  private def tMeta(post: Post) = {
    p(cls := "metadata",
      post.date.map(timeFull).getOrElse(""),
      frag(post.modified.map(timeFull).toList: _*),
      categoriesSpan(post),
      frag(post.folder().map(f => span(cls := "category")(stringFrag(s" in $f"))).toList: _*)
    )
  }

  private def categoriesSpan(post: Post) = {
    span(cls := "category")((post.categories() ++ post.people()).map(c => stringFrag(s" $c ")): _*)
  }

  private def timeFull(date: ScitzenDateTime) = {
    //need time formatter, because to string removes seconds if all zero
    span(cls := "time", date.full)
  }

  private def timeShort(date: ScitzenDateTime) = {
    span(cls := "time",
         stringFrag(
           date.monthDayTime))
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
    val byYear: Map[Int, List[Post]] = posts.groupBy(_.date.get.date.year.toInt)
    frag(byYear.keys.toList.sorted(Ordering[Int].reverse).map { year =>
      val dhs: Seq[Post] = byYear.apply(year).sortBy(_.date.get)(Ordering[ScitzenDateTime].reverse)
      section(cls := "year",
              h1(dhs.head.date.get.date.year),
              frag(dhs.map { post =>
                a(href := s"$path_posts/${post.targetPath}",
                  article(timeShort(post.date.get),
                          span(cls:="title", raw(post.title)),
                          categoriesSpan(post)
                  ))
              }: _*)
      )
    }: _*)
  }

  def makeHtml(stuff: Modifier*): TypedTag[String] =
    html(tHead)(stuff: _*)


  def htmlDocument(tag: Tag): String = "<!DOCTYPE html>" + tag.render


  def makePostHtml(post: Post): String = {
    htmlDocument(makeHtml(tBody(tSingle(post.title, tMeta(post),
                                        raw(new HtmlConverter(scalatags.Text).convert(post.document).render)))))
  }

  def makeIndexOf(posts: List[Post]): String = {
    htmlDocument(makeHtml(tBody(tSection(posts))))
  }


}

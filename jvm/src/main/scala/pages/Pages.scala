package pages

import scalatags.Text.all.{frag, raw, SeqFrag}
import scalatags.Text.attrs.{`type`, charset, cls, content, href, rel, title, name => attrname, id, `for`}
import scalatags.Text.implicits.{Tag, stringAttr, stringFrag}
import scalatags.Text.tags.{a, body, h1, head, header, html, link, meta, p, span, input, label}
import scalatags.Text.tags2.{article, main, section, nav}
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

  def tSection(title: String, content: Frag): Tag = {
    section(h1(id := title, s"$title"), content)
  }

  def postRef(post: Post): Tag = {
    a(href := s"$path_posts/${post.targetPath}",
      article(timeShort(post.date.get),
              span(cls := "title", raw(post.title)),
              categoriesSpan(post)
      ))
  }

  def makeHtml(stuff: Modifier*): TypedTag[String] =
    html(tHead)(stuff: _*)


  def htmlDocument(tag: Tag): String = "<!DOCTYPE html>" + tag.render


  def makePostHtml(post: Post): String = {
    htmlDocument(makeHtml(body(main(tSingle(
      post.title,
      tMeta(post),
      new HtmlConverter(scalatags.Text).convert(post.document))))))
  }

  def makeIndexOf(posts: List[Post]): String = {

    val byYear: Map[Int, List[Post]] = posts.groupBy(_.date.get.date.year.toInt)
    val years = byYear.keys.toList.sorted(Ordering[Int].reverse)

    val sections = years.map { year =>
      val posts = byYear(year).sortBy(_.date.get)(Ordering[ScitzenDateTime].reverse)
      val postList = SeqFrag(posts.map(postRef))
      tSection(year.toString, postList)
    }

    htmlDocument(makeHtml(body(
      cls := "index",
      input(`type` := "checkbox", id := "nav-switch"),
      nav(label(`for` := "nav-switch", raw("""
      <svg viewBox="0 0 48 48">
        <g stroke="black" stroke-width="4" stroke-linecap="round">
        <path d="M 6 12 H 42" />
        <path d="M 6 24 H 42" />
        <path d="M 6 36 H 42" />
        </g>
      </svg>
      """)))(
        years.map(y => a(href := s"#$y", y.toString))),
      main(SeqFrag(sections)))))
  }


}

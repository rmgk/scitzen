package scitzen.cli

import scalatags.Text.all.{SeqFrag, frag, raw}
import scalatags.Text.attrs.{`for`, `type`, charset, cls, content, href, id, lang, rel, title, name => attrname}
import scalatags.Text.implicits.{Tag, stringAttr, stringFrag}
import scalatags.Text.tags.{a, body, h1, head, html, input, label, link, meta, p, span}
import scalatags.Text.tags2.{article, main, nav, section}
import scalatags.Text.{Frag, Modifier, TypedTag}
import scitzen.converter.Post
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

  def tMeta(post: Post) = {
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

  private def tSingle(content: Frag*) = {
    article(cls := "fullpost",
            content
    )
  }

  def tSection(title: String, content: Frag): Tag = {
    section(h1(id := title, s"$title"), content)
  }

  def postRef(post: Post): Tag = {
    a(href := s"$path_posts/${post.targetPath}",
      article(timeShort(post.date.get),
              span(cls := "title", post.title),
              categoriesSpan(post)
      ))
  }

  def makeHtml(stuff: Modifier*): TypedTag[String] =
    html(tHead)(stuff: _*)


  def htmlDocument(tag: Tag): String = "<!DOCTYPE html>" + tag.render


  def wrapContentHtml(language: String, content: Frag): String = {
    htmlDocument(makeHtml(body(main(tSingle(
      content
      )(if (language.nonEmpty) lang := language else frag())
    ))))
  }


  def makeIndexOf(posts: List[Post]): String = {

    posts.foreach { p =>
      if (p.date.isEmpty)
        scribe.warn(s"${p.sourcePath} has no date (${p.document.blocks})")
    }

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

package scitzen.bibliography

import scalatags.Text.all.{Frag, a, cls, frag, href, p, s, stringFrag, given}

case class Author(givenName: Option[String], familyName: Option[String]) {
  def full: String = givenName.fold("")(_ + " ") + familyName.getOrElse("")
}

case class BibEntry(
    id: String,
    authors: List[Author],
    title: Option[String],
    year: Option[Int],
    container: Option[String],
    `type`: String,
    url: Option[String],
    issue: Option[String],
    citekey: Option[String] = None,
):
  def formatAuthors: Option[String] =
    val res = authors.map(_.full).mkString(", ")
    if res.nonEmpty then Some(res) else None
  def formatHtmlCitation: Frag =
    def line(name: String, elems: Option[String | Frag]*): Option[Frag] =
      val terminator = stringFrag(". ")
      val inside: Seq[Frag] = elems.flatten.flatMap {
        case s: String => List(stringFrag(s.stripSuffix(".")), terminator)
        case f: Frag   => List(f, terminator)
      }
      if inside.isEmpty then None else Some(p(cls := name, inside))
    frag(
      line("authors", formatAuthors, year.map(_.toString)),
      line("title", title.map(t => url.fold(t)(u => a(href := u, t.stripSuffix("."))))),
      line("container", container, issue)
    )
  def authorYear: Option[String] =
    for
      author <- authors.headOption
      name   <- author.familyName
      year   <- year
    yield s"$name$year"

  def headerstring: String =
    val authors = formatAuthors.fold("")(a => s"$a ")
    s"$authors${title.fold("")(t => s"»$t«")}"

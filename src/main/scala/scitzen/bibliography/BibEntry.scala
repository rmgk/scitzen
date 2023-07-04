package scitzen.bibliography

import de.rmgk.delay.Sync
import scitzen.html.sag
import scitzen.html.sag.{Recipe, Sag, SagContentWriter}

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
  def formatHtmlCitation: Recipe =
    def line(name: String, elems: Option[String | Recipe]*): Recipe = Sync:
      elems.flatten match
        case Nil => ()
        case insides =>
          val terminator = ". "
          Sag.p(
            `class` = name,
            Sync:
              insides.foreach {
                case s: String =>
                  sag.write(s.stripSuffix("."))
                  sag.write(terminator)
                case f: Recipe =>
                  f.run
                  sag.write(terminator)
              }
          ).run
    Sync:
      line("authors", formatAuthors, year.map(_.toString)).run
      line("title", title.map(t => url.fold(t)(u => Sag.a(href = u, t.stripSuffix("."))))).run
      line("container", container, issue).run

  def authorYear: Option[String] =
    for
      author <- authors.headOption
      name   <- author.familyName
      year   <- year
    yield s"$name$year"

  def headerstring: String =
    val authors = formatAuthors.fold("")(a => s"$a ")
    s"$authors${title.fold("")(t => s"»$t«")}"

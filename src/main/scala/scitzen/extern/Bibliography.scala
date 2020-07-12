package scitzen.extern

import java.nio.charset.StandardCharsets

import better.files.File
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scalatags.Text.all._

object Bibliography {

  case class Author(givenName: Option[String], familyName: Option[String]) {
    def full = givenName.fold("")(_ + " ") + familyName.getOrElse("")
  }

  case class BibEntry(
      id: String,
      authors: List[Author],
      title: Option[String],
      year: Option[Int],
      container: Option[String],
      `type`: String,
      citekey: Option[String] = None
  ) {
    def formatAuthors: Option[String] = {
      val res = authors.map(_.full).mkString(", ")
      if (res.nonEmpty) Some(res) else None
    }
    def format: Frag = frag(formatAuthors, ". ", br, em(title), ". ", br, container, ". ", year, ". ")
  }

  case class CiteprocDate(`date-parts`: List[List[Int]]) {
    def year: Option[Int] = `date-parts`.flatten.headOption
  }
  case class CiteprocAuthor(family: Option[String], `given`: Option[String]) {
    def toAuthor: Author = Author(`given`, family)
  }
  case class CiteprocEntry(
      id: String,
      author: List[CiteprocAuthor],
      issued: Option[CiteprocDate],
      `container-title`: Option[String],
      `type`: String,
      title: Option[String]
  ) {
    def toBibEntry: BibEntry =
      BibEntry(
        id = id,
        authors = author.map(_.toAuthor),
        title = title,
        year = issued.flatMap(_.year),
        container = `container-title`,
        `type` = `type`
      )
  }

  val citeprocCodec: JsonValueCodec[List[CiteprocEntry]] = JsonCodecMaker.make

  def parse(cacheDir: File)(source: File): List[BibEntry] = {
    val hash = scitzen.extern.Hashes.sha1hex(source.contentAsString.getBytes(StandardCharsets.UTF_8))
    cacheDir.createDirectories()
    val cachefile = cacheDir / (hash + ".json")
    if (!cachefile.exists) {
      new ProcessBuilder("pandoc-citeproc", "--bib2json", source.pathAsString)
        .inheritIO()
        .redirectOutput(cachefile.toJava).start().waitFor()
    }

    val entries = readFromStream(cachefile.newInputStream)(citeprocCodec)

    entries.map { _.toBibEntry }
  }
}

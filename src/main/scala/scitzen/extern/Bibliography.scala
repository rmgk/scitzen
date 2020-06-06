package scitzen.extern

import java.nio.charset.StandardCharsets

import better.files.File
import scalatags.Text.all._

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

object Bibliography {


  case class Author(given: Option[String], family: Option[String]) {
    def full = given.fold("")(_ + " ") + family.getOrElse("")
  }


  case class BibEntry(id: String,
                      authors: List[Author],
                      title: Option[String],
                      year: Option[Int],
                      container: Option[String],
                      `type`: String) {
    def formatAuthors: Option[String] = {
      val res = authors.map(_.full).mkString(", ")
      if (res.nonEmpty) Some(res) else None
    }
    def format: Frag = frag(formatAuthors,
                            ". ",
                            br,
                            em(title),
                            ". ",
                            br,
                            container,
                            ". ",
                            year,
                            ". ")
  }


  case class CiteprocDate(`date-parts`: List[List[Int]]) {
    def year: Option[Int] = `date-parts`.flatten.headOption
  }
  case class CiteprocAuthor(family: Option[String], given: Option[String]) {
    def toAuthor: Author = Author(given, family)
  }
  case class CiteprocEntry(id: String, author: List[CiteprocAuthor], issued: CiteprocDate, `container-title`: Option[String], `type`: String, title: Option[String]) {
    def toBibEntry: BibEntry =
      BibEntry(id = id,
               authors = author.map(_.toAuthor),
               title = title,
               year = issued.year,
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
      new ProcessBuilder("pandoc-citeproc",
                         "--bib2json",
                         source.pathAsString)
      .inheritIO()
      .redirectOutput(cachefile.toJava).start().waitFor()
    }

    val entries = readFromStream(cachefile.newInputStream)(citeprocCodec)

    entries.map {_.toBibEntry}
  }
}

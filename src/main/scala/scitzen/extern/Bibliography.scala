package scitzen.extern

import java.nio.charset.StandardCharsets

import better.files.File
import scalatags.Text.all._

import scitzen.compat.CiteProcCodecs._

object Bibliography {

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
    def format: Frag =
      frag(code(citekey), " ", formatAuthors, ". ", br, em(title), ". ", br, container, ". ", year, ". ")
  }

  def citeprocToBib(citeprocEntry: CiteprocEntry) = {
    import citeprocEntry._
    BibEntry(
      id = id,
      authors = author.map(_.toAuthor),
      title = title,
      year = issued.flatMap(_.year),
      container = `container-title`,
      `type` = `type`
      )
  }


  def parse(cacheDir: File)(source: File): List[BibEntry] = {
    val hash = scitzen.extern.Hashes.sha1hex(source.contentAsString.getBytes(StandardCharsets.UTF_8))
    cacheDir.createDirectories()
    val cachefile = cacheDir / (hash + ".json")
    if (!cachefile.exists) {
      new ProcessBuilder("pandoc-citeproc", "--bib2json", source.pathAsString)
        .inheritIO()
        .redirectOutput(cachefile.toJava).start().waitFor()
    }

    val entries = com.github.plokhotnyuk.jsoniter_scala.core.readFromStream(cachefile.newInputStream)(citeprocCodec)

    entries.map { citeprocToBib }
  }
}

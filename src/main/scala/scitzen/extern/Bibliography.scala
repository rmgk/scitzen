package scitzen.extern

import java.nio.charset.StandardCharsets
import better.files.File
import scalatags.Text.all._
import scitzen.compat.Codecs._
import scitzen.generic.Project
import cats.implicits._

object Bibliography:

  case class BibEntry(
      id: String,
      authors: List[Author],
      title: Option[String],
      year: Option[Int],
      container: Option[String],
      `type`: String,
      citekey: Option[String] = None
  ):
    def formatAuthors: Option[String] =
      val res = authors.map(_.full).mkString(", ")
      if res.nonEmpty then Some(res) else None
    def format: Frag =
      frag(code(citekey), " ", formatAuthors, ". ", br, em(title), ". ", br, container, ". ", year, ". ")

  def citeprocToBib(citeprocEntry: CiteprocEntry) =
    import citeprocEntry._
    BibEntry(
      id = id,
      authors = author.map(_.toAuthor),
      title = title,
      year = issued.flatMap(_.year),
      container = `container-title`,
      `type` = `type`
    )

  def parse(cacheDir: File)(source: File): List[BibEntry] =
    val hash = scitzen.extern.Hashes.sha1hex(source.contentAsString.getBytes(StandardCharsets.UTF_8))
    cacheDir.createDirectories()
    val cachefile = cacheDir / (hash + ".json")
    if !cachefile.exists then
      new ProcessBuilder("pandoc-citeproc", "--bib2json", source.pathAsString)
        .inheritIO()
        .redirectOutput(cachefile.toJava).start().waitFor()

    val entries = com.github.plokhotnyuk.jsoniter_scala.core.readFromStream(cachefile.newInputStream)(citeprocCodec)

    entries.map { citeprocToBib }

  def makeBib(project: Project): Map[String, Bibliography.BibEntry] =
    project.bibfile.map { path =>
      Bibliography.parse(project.cacheDir)(path)
    }.getOrElse(Nil).sortBy(be => be.authors.map(_.familyName)).zipWithIndex.map {
      case (be, i) => be.id -> be.copy(citekey = Some((i + 1).toString))
    }.toMap

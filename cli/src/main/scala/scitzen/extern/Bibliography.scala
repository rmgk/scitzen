package scitzen.extern

import better.files.*
import cats.implicits.*
import de.undercouch.citeproc.bibtex.{BibTeXConverter, BibTeXItemDataProvider}
import de.undercouch.citeproc.csl.CSLItemData
import de.undercouch.citeproc.helper.json.{StringJsonBuilder, StringJsonBuilderFactory}
import org.jbibtex.{BibTeXDatabase, BibTeXEntry, BibTeXParser, Key, LaTeXParser, LaTeXPrinter}
import scalatags.Text.all.*
import scitzen.compat.Codecs.*
import scitzen.generic.Project
import cats.syntax.monoid.*

import java.io.FileInputStream
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.*

object Bibliography:

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
      citekey: Option[String] = None
  ):
    def formatAuthors: Option[String] =
      val res = authors.map(_.full).mkString(", ")
      if res.nonEmpty then Some(res) else None
    def format: Frag =
      def line(name: String, elems: Option[Frag]*): Option[Frag] =
        val terminator = stringFrag(". ")
        val inside: Seq[Frag] = elems.flatten.flatMap{v => List(v, terminator)}
        if inside.isEmpty then None else Some(p(cls:=name, inside))
      frag(line("authors", formatAuthors.map(stringFrag), year.map(_.toString)),
           line("title", title.map(t => url.fold(stringFrag(t))(u => a(href := u, t)))),
           line("container", container.map(stringFrag)))
    def authorYear: Option[String] =
      authors.headOption.flatMap(_.familyName).combine(year.map(_.toString))

  def citeprocToBib(entry: CSLItemData) =
    BibEntry(
      id = entry.getId,
      authors = Option(entry.getAuthor).map(_.toList).getOrElse(Nil).map(a =>
        Author(Option(a.getGiven), Option(a.getFamily))
      ).toList,
      title = Option(entry.getTitle),
      year = Option(entry.getIssued).flatMap(_.getDateParts.flatten.headOption),
      container = Option(entry.getContainerTitle),
      `type` = Option(entry.getType).map(_.toString).getOrElse("unknown-csl-type"),
      url = Option(entry.getURL)
    )

  def parse(cacheDir: File)(source: File): List[BibEntry] = {
    val converter = BibTeXConverter()
    val db = source.fileInputStream(converter.loadDatabase)
    val items     = converter.toItemData(db).asScala
    //val published = extractHowpublished(db)
    items.valuesIterator.map { citeprocToBib }.toList
  }

  def makeBib(project: Project): Map[String, Bibliography.BibEntry] =
    project.bibfile.map { path =>
      Bibliography.parse(project.cacheDir)(path)
    }.getOrElse(Nil).sortBy(be => be.authors.map(_.familyName)).zipWithIndex.map {
      case (be, i) => be.id -> be.copy(citekey = Some(be.authorYear.getOrElse((i + 1).toString)))
    }.toMap

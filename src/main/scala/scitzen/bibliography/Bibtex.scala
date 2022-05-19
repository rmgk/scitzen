package scitzen.bibliography

import better.files.given
import cats.implicits.given
import de.undercouch.citeproc.bibtex.{BibTeXConverter, BibTeXItemDataProvider}
import de.undercouch.citeproc.csl.CSLItemData
import de.undercouch.citeproc.helper.json.{StringJsonBuilder, StringJsonBuilderFactory}
import org.jbibtex.{BibTeXDatabase, BibTeXEntry, BibTeXParser, Key, LaTeXParser, LaTeXPrinter}
import scitzen.compat.Codecs.given
import scitzen.generic.Project

import java.io.{FileInputStream, InputStream}
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.*

object Bibtex:

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
      url = Option(entry.getURL),
      issue = Option(entry.getIssue),
    )

  def parse(source: InputStream): List[BibEntry] = {
    val converter = BibTeXConverter()
    val db        = converter.loadDatabase(source)
    source.close()
    val items = converter.toItemData(db).asScala
    items.valuesIterator.map { citeprocToBib }.toList
  }

  def makeBib(project: Project): Map[String, BibEntry] =
    project.bibfile.map { path =>
      path.fileInputStream(Bibtex.parse)
    }.getOrElse(Nil).sortBy(be => be.authors.map(_.familyName)).zipWithIndex.map {
      case (be, i) => be.id -> be.copy(citekey = Some(be.authorYear.getOrElse((i + 1).toString)))
    }.toMap

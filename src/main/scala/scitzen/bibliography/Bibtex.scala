package scitzen.bibliography

import de.undercouch.citeproc.bibtex.BibTeXConverter
import de.undercouch.citeproc.csl.CSLItemData

import java.io.InputStream
import scala.jdk.CollectionConverters.*
import scala.math.Ordering.Implicits.seqOrdering

object Bibtex:

  def citeprocToBib(entry: CSLItemData) =
    BibEntry(
      id = entry.getId,
      authors = Option(entry.getAuthor).map(_.toList).getOrElse(Nil).map(a =>
        Author(Option(a.getGiven), Option(a.getFamily))
      ).toList,
      title = Option(entry.getTitle),
      year = Option(entry.getIssued).flatMap(i => Option(i.getDateParts).flatMap(_.flatten.headOption)),
      container = Option(entry.getContainerTitle),
      `type` = Option(entry.getType).map(_.toString).getOrElse("unknown-csl-type"),
      url = Option(entry.getURL).orElse(Option(entry.getDOI).map(doi => s"https://doi.org/$doi")),
      issue = Option(entry.getIssue),
    )

  def parse(source: InputStream): List[BibEntry] = {
    val converter = BibTeXConverter()
    val db        = converter.loadDatabase(source)
    source.close()
    val items = converter.toItemData(db).asScala
    items.valuesIterator.map { citeprocToBib }.toList
  }

  def makeBib(allbibs: Seq[BibEntry]): Map[String, BibEntry] =
    allbibs.sortBy(be =>
      be.authors.map(_.familyName)
    ).zipWithIndex.map {
      case (be, i) => be.id -> be.copy(citekey = Some(be.authorYear.getOrElse((i + 1).toString)))
    }.toMap

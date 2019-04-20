package scitzen.cli

import java.nio.file.Path

import better.files.File
import de.undercouch.citeproc.bibtex.BibTeXConverter

import scala.collection.JavaConverters._

object Bibliography {


  case class Author(given: String, family: String) {
    def full = s"$given $family"
  }

  case class BibEntry(id: String,
                      authors: List[Author],
                      title: Option[String],
                      year: Option[Int],
                      container: Option[String]) {
    def formatAuthors: Option[String] = {
      val res = authors.map(_.full).mkString(", ")
      if (res.nonEmpty) Some(res) else None
    }
    def format: String = Seq(formatAuthors, year, title, container).flatten.mkString(". ")
  }

  def parse(source: Path): List[BibEntry] = {
    val is = File(source).newFileInputStream
    val btc = new BibTeXConverter()
    val db = try {btc.loadDatabase(is)} finally {is.close()}
    val itemdata = btc.toItemData(db)
    itemdata.asScala.map { case (id, data) =>
      val authors = Option(data.getAuthor)
                    .fold(List.empty[Author])(_.map(a => Author(a.getGiven, a.getFamily)).toList)
      val title = Option(data.getTitle)
      val date = Option(data.getEventDate)
                 .flatMap(_.getDateParts.headOption).flatMap(_.headOption)
      val container = Option(data.getContainerTitle)
      BibEntry(id, authors, title, date, container)
    }.toList

  }
}

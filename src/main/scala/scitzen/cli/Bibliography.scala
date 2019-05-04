package scitzen.cli

import better.files.File
import scalatags.Text.all._

import scala.util.Try

object Bibliography {


  case class Author(given: String, family: String) {
    def full = s"$given $family"
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


  def parse(source: File): List[BibEntry] = {
    val jsonStr = scala.sys.process.Process(Seq("pandoc-citeproc",
                                                "--bib2json",
                                                source.pathAsString)).!!
    ujson.read(jsonStr).arr.iterator.map { e =>
      val obj = e.obj

      def opt(v: ujson.Value) = Try {v.str}.toOption

      val authors = {
        obj.get("author").toList.flatMap {
          _.arr.map(a => Author(given = a.obj("given").str, family = a.obj("family").str))
        }
      }

      val year = {
        for {
          iss <- obj.get("issued")
          parts <- iss.obj.get("date-parts")
          first <- parts.arr.headOption
          year <- first.arr.headOption
        } yield year.num.toInt
      }

      BibEntry(id = obj("id").str,
               authors = authors,
               title = opt(obj("title")),
               year = year,
               container = obj.get("container-title").flatMap(opt),
               `type` = obj("type").str
               )
    }.toList
  }
}

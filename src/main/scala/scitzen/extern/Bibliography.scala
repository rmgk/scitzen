package scitzen.extern

import java.nio.charset.StandardCharsets

import better.files.File
import scalatags.Text.all._

import scala.util.Try
import scala.util.control.NonFatal

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


  def parse(cacheDir: File)(source: File): List[BibEntry] = {
    val jsonStr = {
      val hash = scitzen.extern.Hashes.sha1hex(source.contentAsString.getBytes(StandardCharsets.UTF_8))
      cacheDir.createDirectories()
      val cachefile = cacheDir / (hash + ".json")
      if (cachefile.exists) {cachefile.contentAsString}
      else {
        val res = scala.sys.process.Process(Seq("pandoc-citeproc",
                                                "--bib2json",
                                                source.pathAsString)).!!
        cachefile.write(res)
        res
      }
    }
    ujson.read(jsonStr).arr.iterator.map { e =>
      val obj = e.obj

      def opt(v: ujson.Value) = Try {v.str}.toOption

      val id = obj("id").str
      try {
        val authors = {
          obj.get("author").toList.flatMap {
            _.arr.map(a => Author(given = a.obj.get("given").map(_.str), family = a.obj.get("family").map(_.str)))
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

        BibEntry(id = id,
                 authors = authors,
                 title = opt(obj("title")),
                 year = year,
                 container = obj.get("container-title").flatMap(opt),
                 `type` = obj("type").str
                 )
      } catch {
        case NonFatal(e) =>
          scribe.error(s"error while converting bib $id")
          throw e
      }
    }.toList
  }
}

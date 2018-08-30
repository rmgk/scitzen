package vitzen

import java.nio.charset.{Charset, StandardCharsets}
import java.time.format.DateTimeFormatter
import java.time.{Instant, OffsetDateTime, ZoneOffset}

import better.files.File
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.parser.Parser

import scala.collection.JavaConverters._

case class Entry(title: String, date: OffsetDateTime, text: String, folder: Option[String], tags: List[String])

object DiaroImport {

  def main(args: Array[String]): Unit = {
    implicit val charset: Charset = StandardCharsets.UTF_8
    val f = File(args(0))
    val doc = Jsoup.parse(f.contentAsString(), "", Parser.xmlParser())
    val folders = tableToMap(doc.selectFirst("table[name='diaro_folders']"))
    val tags = tableToMap(doc.selectFirst("table[name='diaro_tags']"))

    def toEntry(element: Element) = {
      val title = {
        val t = element.selectFirst("title").text()
        if (t.isEmpty) "(Untitled)" else t
      }
      val text = element.selectFirst("text").wholeText()
      val date = {
        val tz = element.selectFirst("tz_offset").text()
        val Array(hr, min) = tz.split(':').map(_.toInt)
        val time = element.selectFirst("date").text().toLong
        Instant.ofEpochMilli(time).atOffset(ZoneOffset.ofHoursMinutes(hr, min))
      }
      val folder = {
        val uid = element.selectFirst("folder_uid").text()
        if (uid.isEmpty) None
        else {
          folders.get(uid)
        }
      }
      val tagNames = {
        element.selectFirst("tags").text().split(',').filter(_.nonEmpty).flatMap(tags.get).toList
      }
      Entry(title, date, text, folder, tagNames)
    }

    val entries = doc.select("table[name=diaro_entries] r").iterator().asScala.map {toEntry}

    val outdir = File("converted_output")
    outdir.delete(swallowIOExceptions = true)

    entries.foreach{ e =>
      val date = e.date.format(DateTimeFormatter.ISO_LOCAL_DATE)
      val time = e.date.format(DateTimeFormatter.ISO_LOCAL_TIME)
      val url = e.title.replaceAll("[^\\p{L}]", "-")
      val slug = s"${date}T${time}_$url.adoc"
      val target = outdir / e.folder.getOrElse("") / slug
      target.parent.createDirectories()
      target.appendLine(s"= ${e.title}")
      target.appendLine(s":revdate: $date $time")
      e.folder.foreach{f => target.appendLine(s":folder: $f") }
      if (e.tags.nonEmpty) target.appendLine(s":categories: ${e.tags.mkString(", ")}")
      target.appendLine("")
      target.appendLine("[.import]")
      target.appendLine("....")
      target.append(e.text)
      target.appendLine("")
      target.appendLine("....")
      println(slug)
    }

  }


  private def tableToMap(table: Element) = {
    table.select("r")
    .iterator().asScala.map { f =>
      val id = f.selectFirst("uid").text()
      val title = f.selectFirst("title").text()
      (id, title)
    }.toMap
  }
}


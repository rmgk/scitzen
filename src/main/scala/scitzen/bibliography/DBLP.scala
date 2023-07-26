package scitzen.bibliography

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.jsoup.Jsoup
import scitzen.bibliography.DBLPApi.Outer
import scitzen.cli.Format
import scitzen.parser.Parse

import java.net.http.HttpResponse.BodyHandlers
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.FutureConverters.*
import HttpUtil.{client, query}
import scitzen.parser.Biblet

object DBLPApi:
  case class Outer(result: Result)
  case class Result(hits: Hits)
  case class Hits(hit: List[Hit] = Nil)
  case class Hit(info: Info)
  case class Info(
      key: String,
      title: String,
      ee: String = ""
  )
  case class Author(`@pid`: String, text: String)

object DBLP extends BibtexProvider:

  val codec: JsonValueCodec[Outer] = JsonCodecMaker.make

  def lookup(uri: String): Option[Biblet] =
    if !uri.startsWith("DBLP:") then None
    else
      val key = uri.stripPrefix("DBLP:")
      HttpUtil.getBody(HttpUtil.query(s"https://dblp.org/rec/$key.bib")).map: res =>
        Parse.bibfileUnwrap(res.getBytes(StandardCharsets.UTF_8)).head

  def lookupFormatted(key: String): List[String] =
    val res   = HttpUtil.client.send(HttpUtil.query(s"https://dblp.org/rec/$key.bib"), BodyHandlers.ofInputStream())
    val items = Bibtex.parse(res.body())

    items.map { bi =>
      List(Some(s"== ${bi.headerstring}"), Some(s"dblp = $key"), bi.url.map(u => s"url = $u")).flatten.mkString("\n")
    }

  val AcmRx           = """//dl\.acm\.org/""".r.unanchored
  val ArxiveRx        = """//arxiv\.org/""".r.unanchored
  val CambridgeCoreRx = """//www\.cambridge\.org/""".r.unanchored
  val DagstuhlRx      = """//drops\.dagstuhl\.de/""".r.unanchored

  def search(q: String): List[DBLPApi.Info] =
    val res = HttpUtil.client.send(
      HttpUtil.query(s"https://dblp.org/search/publ/api", Map("format" -> "json", "q" -> q)),
      BodyHandlers.ofString()
    )
    // println(s"json: ${res.body()}")
    readFromString[DBLPApi.Outer](res.body())(using codec).result.hits.hit.map(_.info)

  def downloadPDFAndFormat(q: String) =
    val hits = search(q)
    hits.map { info =>
      val format = lookupFormatted(info.key)
      println(s"info url: ${info.ee}")
      val pdfpage = HttpUtil.client.send(HttpUtil.query(info.ee), BodyHandlers.ofInputStream())
      println(s"final uri: ${pdfpage.uri()}")
      val soup = Jsoup.parse(pdfpage.body(), StandardCharsets.UTF_8.toString, pdfpage.uri().toString)
      val pdfurl = pdfpage.uri().toString match
        case AcmRx()           => soup.select("a.btn.red").attr("abs:href")
        case ArxiveRx()        => soup.select("a.download-pdf").attr("abs:href")
        case CambridgeCoreRx() => soup.select("meta[name=citation_pdf_url]").attr("content")
        case DagstuhlRx()      => soup.select("table:contains(pdf-format) a[itemprop=url]").attr("href")
      println(s"pdfurl: $pdfurl")
      val filename = Format.sluggify(if info.title.endsWith(".") then info.title.dropRight(1) else info.title)
      val pdffile  = Path.of(s"$filename.pdf")
      client.send(query(pdfurl), BodyHandlers.ofFile(pdffile))

      val sha1 = scitzen.extern.Hashes.sha1hex(Files.readAllBytes(pdffile))
      s"${format.head}\nfile = $sha1"
    }

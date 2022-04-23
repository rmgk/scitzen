package scitzen.bibliography

import better.files.{File, given}
import org.jsoup.Jsoup
import scitzen.cli.Format

import java.net.{CookieHandler, CookieManager, URI, URLEncoder}
import java.net.http.{HttpClient, HttpRequest}
import java.net.http.HttpResponse.BodyHandlers
import java.nio.charset.StandardCharsets
import java.time.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.jdk.FutureConverters.*
import upickle.default.ReadWriter

import java.net.http.HttpClient.Redirect

object DBLPApi:
  case class Outer(result: Result) derives ReadWriter
  case class Result(hits: Hits) derives ReadWriter
  case class Hits(hit: List[Hit]) derives ReadWriter
  case class Hit(info: Info) derives ReadWriter
  case class Info(
      key: String,
      title: String,
      ee: String
  ) derives ReadWriter
  case class Authors(author: List[Author]) derives ReadWriter
  case class Author(`@pid`: String, text: String) derives ReadWriter

object DBLP:

  val client = HttpClient.newBuilder.connectTimeout(Duration.ofSeconds(30)).followRedirects(
    Redirect.ALWAYS
  ).cookieHandler(new CookieManager()).build
  def query(url: String, params: Map[String, String] = Map.empty): HttpRequest =
    val query = params.map((k, v) => s"$k=${URLEncoder.encode(v, StandardCharsets.UTF_8)}").mkString("&")
    val uri   = URI.create(s"$url${if query.nonEmpty then "?" else ""}$query")
    println(s"uri: $uri")
    HttpRequest.newBuilder.uri(uri).timeout(Duration.ofSeconds(30)).build

  def lookup(key: String) =
    val res   = client.send(query(s"https://dblp.org/rec/$key.bib"), BodyHandlers.ofInputStream())
    val items = Bibtex.parse(res.body())

    items.map { bi =>
      List(Some(s"== ${bi.headerstring}"), Some(s"dblp = $key"), bi.url.map(u => s"url = $u")).flatten.mkString("\n")
    }

  val AcmRx = """//dl\.acm\.org/""".r.unanchored
  val ArxiveRx = """//arxiv\.org/""".r.unanchored
  val CambridgeCoreRx = """//www\.cambridge\.org/""".r.unanchored
  val DagstuhlRx = """//drops\.dagstuhl\.de/""".r.unanchored

  def search(q: String) =
    val res = client.send(
      query(s"https://dblp.org/search/publ/api", Map("format" -> "json", "q" -> q)),
      BodyHandlers.ofString()
    )
    println(s"json: ${res.body()}")
    val json = upickle.default.read[DBLPApi.Outer](res.body())
    json.result.hits.hit.map { h =>
      val info   = h.info
      val format = lookup(info.key)
      println(s"info url: ${info.ee}")
      val pdfpage = client.send(query(info.ee), BodyHandlers.ofInputStream())
      println(s"final uri: ${pdfpage.uri()}")
      val soup   = Jsoup.parse(pdfpage.body(), StandardCharsets.UTF_8.toString, pdfpage.uri().toString)
      val pdfurl = pdfpage.uri().toString match
        case AcmRx() => soup.select("a.btn.red").attr("abs:href")
        case ArxiveRx() => soup.select("a.download-pdf").attr("abs:href")
        case CambridgeCoreRx() => soup.select("meta[name=citation_pdf_url]").attr("content")
        case DagstuhlRx() => soup.select("table:contains(pdf-format) a[itemprop=url]").attr("href")
      println(s"pdfurl: $pdfurl")
      val pdfres   = client.send(query(pdfurl), BodyHandlers.ofInputStream())
      val filename = Format.sluggify(if info.title.endsWith(".") then info.title.dropRight(1) else info.title)
      val pdfbody  = pdfres.body()
      val pdffile  = File(s"$filename.pdf")
      pdffile.outputStream.foreach(os => pdfbody.pipeTo(os, 256))
      pdfbody.close()
      val sha1 = scitzen.extern.Hashes.sha1hex(pdffile.byteArray)
      s"${format.head}\nfile = $sha1"
    }
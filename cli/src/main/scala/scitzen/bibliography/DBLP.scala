package scitzen.bibliography

import java.net.URI
import java.net.http.{HttpClient, HttpRequest}
import java.net.http.HttpResponse.BodyHandlers
import java.time.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.jdk.FutureConverters.*

object DBLP:
  def lookup(key: String) =

    val client = HttpClient.newBuilder.connectTimeout(Duration.ofSeconds(20)).build

    val request =
      HttpRequest.newBuilder.uri(URI.create(s"https://dblp.org/rec/$key.bib")).timeout(Duration.ofSeconds(10)).build

    client.sendAsync(request, BodyHandlers.ofInputStream()).asScala.map { res =>
      val items = Bibtex.parse(res.body())

      items.map { bi =>
        List(Some(s"== ${bi.headerstring}"), Some(s"dblp = $key"), bi.url.map(u => s"url = $u")).flatten.mkString("\n")
      }
    }

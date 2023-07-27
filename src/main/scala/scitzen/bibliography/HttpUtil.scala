package scitzen.bibliography

import java.net.http.HttpRequest
import java.net.http.HttpClient.Redirect
import java.net.http.HttpResponse.{BodyHandler, BodyHandlers}
import java.net.http.HttpClient
import java.net.{CookieManager, URI, URLEncoder}
import java.nio.charset.StandardCharsets
import java.time.Duration

object HttpUtil {
  val client = HttpClient.newBuilder.connectTimeout(Duration.ofSeconds(30)).followRedirects(
    Redirect.ALWAYS
  ).cookieHandler(new CookieManager()).build
  def query(url: String, params: Map[String, String] = Map.empty): HttpRequest =
    val query = params.map((k, v) => s"$k=${URLEncoder.encode(v, StandardCharsets.UTF_8)}").mkString("&")
    val uri = URI.create(s"$url${
        if query.nonEmpty then "?"
        else ""
      }$query")
    println(s"uri: $uri")
    HttpRequest.newBuilder.uri(uri).timeout(Duration.ofSeconds(30)).build

  def getBody[T](req: HttpRequest, bodyHandler: BodyHandler[T] = BodyHandlers.ofString()): Option[T] =
    val res = client.send(req, bodyHandler)
    if res.statusCode() != 200
    then None
    else Some(res.body())

}

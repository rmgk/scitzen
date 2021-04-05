package scitzen.extern

import java.net.http.HttpClient
import java.net.URI
import java.net.http.HttpRequest
import java.net.http.HttpResponse.BodyHandlers
import java.time.Duration
import scala.jdk.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global

object DBLP {
  def lookup(key: String) = {

    val client = HttpClient.newBuilder.connectTimeout(Duration.ofSeconds(20)).build

    val request =
      HttpRequest.newBuilder.uri(URI.create(s"https://dblp.org/rec/$key.bib")).timeout(Duration.ofSeconds(10)).build

    val response = client.send(request, BodyHandlers.ofString)
    System.out.println(response.statusCode)
    System.out.println(response.body)

    client.sendAsync(request, BodyHandlers.ofString).asScala.foreach{r => println(r.body())}

  }
}

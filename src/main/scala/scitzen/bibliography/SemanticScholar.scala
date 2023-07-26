package scitzen.bibliography

import java.net.http.HttpResponse.BodyHandlers
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scitzen.parser.{Biblet, Parse}

import java.nio.charset.StandardCharsets

object SemanticScholar extends BibtexProvider {

  case class CitationStyles(bibtex: String)
  case class ApiResult(paperId: String, citationStyles: CitationStyles)

  given JsonValueCodec[ApiResult] = JsonCodecMaker.make

  override def lookup(uri: String): Option[Biblet] =
    val cid = uri.stripPrefix("S2CID:")
    if cid == uri then None
    else
      HttpUtil.getBody(
        HttpUtil.query(s"https://api.semanticscholar.org/graph/v1/paper/CorpusID:${cid}?fields=citationStyles"),
        BodyHandlers.ofByteArray()
      ).map: bytes =>
        val bibtex = readFromArray[ApiResult](bytes).citationStyles.bibtex
        val biblet = Parse.bibfileUnwrap(bibtex.getBytes(StandardCharsets.UTF_8)).head
        biblet.copy(id = uri)

}

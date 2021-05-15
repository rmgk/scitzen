package scitzen.compat

import scitzen.sast._
import toml.Codecs._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}

case class ProjectConfig(
    output: String = "scitzen/out",
    cache: String = "scitzen/cache",
    stopwords: String = "scitzen",
    format: List[String] = Nil,
    outputType: List[String] = Nil,
    revealTemplate: Option[String] = None,
    definitions: Map[String, String] = Map.empty,
    texTemplate: Option[String] = None,
    notes: Option[String] = None,
)

object Config {
  def parse(content: String) = {
    toml.Toml.parseAs[ProjectConfig](content)
  }

}

object Codecs {

  implicit val SastEncoder: JsonValueCodec[List[Sast]] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

  val mapCodec: JsonValueCodec[Map[String, String]] = JsonCodecMaker.make

  case class Reference(file: String, start: Int, end: Int)

  implicit val rferenceRW: JsonValueCodec[Map[String, List[Reference]]] = JsonCodecMaker.make

  case class Author(givenName: Option[String], familyName: Option[String]) {
    def full: String = givenName.fold("")(_ + " ") + familyName.getOrElse("")
  }

  case class CiteprocDate(`date-parts`: List[List[Int]]) {
    def year: Option[Int] = `date-parts`.flatten.headOption
  }
  case class CiteprocAuthor(family: Option[String], `given`: Option[String]) {
    def toAuthor: Author = Author(`given`, family)
  }

  case class CiteprocEntry(
      id: String,
      author: List[CiteprocAuthor],
      issued: Option[CiteprocDate],
      `container-title`: Option[String],
      `type`: String,
      title: Option[String]
  )

  val citeprocCodec: JsonValueCodec[List[CiteprocEntry]] = JsonCodecMaker.make

}

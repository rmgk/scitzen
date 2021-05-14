package scitzen.compat

import toml.Codecs._

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

package scitzen.generic

import scitzen.parser.{AttributesParser, Parse}
import scitzen.sast.{Attributes, Prov}

case class ProjectConfig(
    output: String,
    cache: Option[String] = None,
    format: List[String] = Nil,
    outputType: List[String] = Nil,
    revealTemplate: Option[String] = None,
    definitions: Map[String, String] = Map.empty,
    texTemplate: Option[String] = None,
    notes: Option[String] = None,
    bibliography: Option[String] = None,
)

object ProjectConfig {
  def parse(content: Array[Byte]): ProjectConfig = {
    val value = Parse.parseResult(content, AttributesParser.configFile, Prov())
    val attrs = Attributes(value)
    ProjectConfig(
      output = attrs.named.getOrElse("output", "output"),
      cache = attrs.named.get("cache"),
      format = attrs.named.getOrElse("format", "").split(',').toList.map(_.trim),
      outputType = attrs.named.getOrElse("outputType", "").split(',').toList.map(_.trim),
      revealTemplate = attrs.named.get("revealTemplate"),
      texTemplate = attrs.named.get("texTemplate"),
      notes = attrs.named.get("notes"),
      bibliography = attrs.named.get("bibliography"),
      definitions = attrs.nestedMap.get("definitions").map(_.named).getOrElse(Map.empty)
    )
  }
}

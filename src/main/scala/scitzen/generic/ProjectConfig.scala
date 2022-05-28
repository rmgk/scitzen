package scitzen.generic

import scitzen.outputs.SastToScimConverter
import scitzen.scipparse.{AttributesParser, Parse}
import scitzen.sast.{Attributes, Directive, Prov, Text}
import scala.util.chaining.scalaUtilChainingOps

case class ProjectConfig(
    output: String,
    cache: String = "scitzen.cache",
    stopwords: String = "scitzen.project",
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
      output = attrs.named.getOrElse("output", "scitzen.out"),
      cache = attrs.named.getOrElse("cache", "scitzen.cache"),
      stopwords = attrs.named.getOrElse("stopwords", "scitzen.project"),
      format = attrs.named.getOrElse("format", "").split(',').toList.map(_.trim),
      outputType = attrs.named.getOrElse("outputType", "").split(',').toList.map(_.trim),
      revealTemplate = attrs.named.get("revealTemplate"),
      texTemplate = attrs.named.get("texTemplate"),
      notes = attrs.named.get("notes"),
      bibliography = attrs.named.get("bibliography"),
      definitions = attrs.nested.get("definitions").map(_.named).getOrElse(Map.empty)
    )
  }
}

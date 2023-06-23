package scitzen.generic

import scitzen.parser.{AttributesParser, Parse}
import scitzen.sast.{Attributes, Prov}

case class ProjectConfig(
    output: String,
    cache: Option[String] = None,
    format: List[String] = Nil,
    outputType: List[String] = Nil,
    revealTemplate: Option[String] = None,
    texTemplate: Option[String] = None,
    notes: Option[String] = None,
    bibliography: Option[String] = None,
    rawAttributes: Attributes = Attributes.empty
)

object ProjectConfig {
  //TODO: generalize this for all directives
  def parse(content: Array[Byte]): ProjectConfig = {
    val value = Parse.parseResult(content, AttributesParser.configFile, Prov())
    val attrs = Attributes(value)
    ProjectConfig(
      output = attrs.plain("output").getOrElse("output"),
      cache = attrs.plain("cache"),
      format = attrs.plain("format").getOrElse("").split(',').toList.map(_.trim),
      outputType = attrs.plain("outputType").getOrElse("").split(',').toList.map(_.trim),
      revealTemplate = attrs.plain("revealTemplate"),
      texTemplate = attrs.plain("texTemplate"),
      notes = attrs.plain("notes"),
      bibliography = attrs.plain("bibliography"),
      rawAttributes = attrs
    )
  }
}

package scitzen.generic

import scitzen.parser.{AttributesParser, Parse}
import scitzen.sast.{Attributes, Prov}

class ProjectConfig(val attrs: Attributes = Attributes.empty):
  def output       = attrs.plain("output").getOrElse("scitzen.out")
  def cache        = attrs.plain("cache")
  def format       = attrs.plain("format").getOrElse("").split(',').toList.map(_.trim)
  def bibliography = attrs.plain("bibliography")
  def katexMacros  = attrs.plain("katexMacros")
  def defaultLanguage  = attrs.plain("language").orElse(attrs.plain("lang"))
  val flags: Flags = Flags.default.apply(attrs.plainList("flags"))


object ProjectConfig {
  def parse(content: Array[Byte]): ProjectConfig = {
    val value = Parse.parseResult(content, AttributesParser.configFile, Prov())
    val attrs = Attributes(value)
    ProjectConfig(attrs)
  }
}

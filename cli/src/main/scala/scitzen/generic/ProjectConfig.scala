package scitzen.generic

import scitzen.outputs.SastToScimConverter
import scitzen.parser.{AttributesParser, Parse}
import scitzen.sast.{Attributes, Macro, Prov, Text}

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
  def parse(content: String): ProjectConfig = {
    Parse.parseResult(content, AttributesParser.noBraces(_), Prov()) match {
      case Left(value) => throw value
      case Right(value) =>
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
          definitions = attrs.namedT.get("definitions") match {
            case Some(Text(List(Macro(_, attributes, _)))) => attributes.namedT.view.mapValues(t => SastToScimConverter.inlineToScim(t.inl).mkString("")).toMap
            case other =>
              println(s"definitions was $other")
              Map.empty
          }
        )
    }
  }
}
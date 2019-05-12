package scitzen.generic

import scitzen.generic.Sast.{AttributedBlock, ParsedBlock, Section}
import scitzen.generic.SastAnalyzes.AnalyzeResult
import scitzen.parser.{Attributes, DateParsingHelper, ScitzenDateTime}

case class Sdoc(sast: Seq[Sast]) {

  lazy val analyzeResult: AnalyzeResult = SastAnalyzes.analyze(sast)

  lazy val named: Map[String, String] = Attributes(analyzeResult.attributes).named

  lazy val language: String = named.getOrElse("lang", "").trim

  lazy val date    : Option[ScitzenDateTime] = named.get("revdate")
                                               .map(v => DateParsingHelper.parseDate(v.trim))
  lazy val modified: Option[ScitzenDateTime] = named.get("modified")
                                               .map(m => DateParsingHelper.parseDate(m.trim))

  lazy val title: Option[String] = sast.headOption.collect { case s : Section => s }.map(_.title.str)

  def targets = analyzeResult.targets

}

object Sdoc {
  def findSections(cont: Seq[Sast]): Seq[Section] = {
    cont.flatMap {
      case s: Section => List(s)
      case AttributedBlock(_, content) => findSections(List(content))
      case ParsedBlock(_, content) => findSections(content)
      case _ => Nil
    }
  }
}

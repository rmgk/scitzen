package scitzen.generic

import scitzen.generic.Sast.{Section, TLBlock}
import scitzen.generic.SastAnalyzes.AnalyzeResult
import scitzen.outputs.SastToTextConverter
import scitzen.parser.{Attributes, DateParsingHelper, ScitzenDateTime}
import cats.implicits._

case class Sdoc(blocks: Seq[TLBlock]) {

  lazy val analyzeResult: AnalyzeResult = SastAnalyzes.analyze(blocks)

  lazy val named: Map[String, String] = Attributes.fromAttributeSeq(analyzeResult.attributes).named

  lazy val language: Option[String] = named.get("language").map(_.trim)

  lazy val date    : Option[ScitzenDateTime] = named.get("revdate")
                                               .map(v => DateParsingHelper.parseDate(v.trim))
  lazy val modified: Option[ScitzenDateTime] = named.get("modified")
                                               .map(m => DateParsingHelper.parseDate(m.trim))

  lazy val title: Option[String] = blocks.headOption.collect { case TLBlock(_, _, s: Section) => s }.map(_.title.str)

  lazy val words: List[String] = SastToTextConverter.convert(blocks)
                                 .flatMap(_.split("[^\\p{L}]+")).toList

  lazy val wordcount: Map[String, Int] =
    words.foldMap(s => Map(s.toLowerCase() -> 1))

  lazy val bigrams: Map[(String, String), Int] = {
    words.sliding(2, 1).toList.foldMap{
      case List(a, b) => Map((a.toLowerCase(), b.toLowerCase()) -> 1)
      case _ => Map()
    }
  }

  def targets = analyzeResult.targets

}

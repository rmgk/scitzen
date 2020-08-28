package scitzen.parser

import cats.implicits._
import fastparse.NoWhitespace._
import fastparse.Parsed.{Failure, Success, TracedFailure}
import fastparse._
import scitzen.sast.{Inline, Prov, Sast}

case class ParsingAnnotation(content: String, failure: TracedFailure) extends Exception

object Parse {

  type Result[T] = Either[ParsingAnnotation, T]

  def parseResult[T](content: String, parser: P[_] => P[T], prov: Prov): Result[T] = {
    val parsed = fastparse.parse(content, { p: P[_] => p.misc("provenanceOffset") = prov; parser(p) })
    parsed match {
      case Success(value, _) => value.asRight
      case f: Failure        =>
        val traced = f.trace()
        scribe.error(
          s"failed to parse ${traced.longMsg}\nwhile parsing: ${content.substring(0, math.min(80, content.length))}..."
        )
        ParsingAnnotation(content, traced).asLeft
    }
  }

  def parserDocument[_: P]: P[Seq[Sast]] = P(BlockParsers.alternatives.rep ~ End)

  def documentUnwrap(blockContent: String, prov: Prov): Seq[Sast] = {
    parseResult(blockContent, parserDocument(_), prov) match {
      case Left(parsingAnnotation) => throw parsingAnnotation
      case Right(res)              => res.toList
    }
  }

  val allInlines = InlineParsers("", End(_))

  def inlineUnwrap(paragraphString: String, prov: Prov): Seq[Inline] = {
    parseResult(paragraphString, allInlines.full(_), prov).toTry.get._1
  }
}

package scitzen.parser

import cats.implicits._
import fastparse.NoWhitespace._
import fastparse.Parsed.{Failure, Success, TracedFailure}
import fastparse._
import scitzen.generic.Sast

object DocumentParsers {
  def document[_: P]: P[Seq[Sast]] = P(BlockParsers.alternatives.rep ~ End)
}

case class ParsingAnnotation(content: String, failure: TracedFailure) extends Exception

object Parse {

  type Result[T] = Either[ParsingAnnotation, T]

  def parseResult[T](content: String, parser: P[_] => P[T], prov: Prov): Result[T] = {
    val parsed = fastparse.parse(content, { p: P[_] => p.misc("provenanceOffset") = prov; parser(p) })
    parsed match {
      case Success(value, index) => value.asRight
      case f: Failure =>
        val traced = f.trace()
        scribe.error(
          s"failed to parse ${traced.longMsg}\nwhile parsing: ${content.substring(0, math.min(80, content.length))}..."
        )
        ParsingAnnotation(content, traced).asLeft
    }
  }

  def document(blockContent: String, prov: Prov): Result[Seq[Sast]] =
    parseResult(blockContent, scitzen.parser.DocumentParsers.document(_), prov)

  def paragraph(paragraphString: String, prov: Prov): Result[Seq[Inline]] =
    parseResult(paragraphString, scitzen.parser.InlineParsers.fullParagraph(_), prov)

  def valueOrThrow[T](result: Result[T]): T = result match {
    case Right(value) => value
    case Left(pa) => throw pa
  }
}

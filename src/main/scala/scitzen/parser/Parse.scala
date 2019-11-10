package scitzen.parser

import cats.implicits._
import fastparse.P
import fastparse.Parsed.{Failure, Success, TracedFailure}

case class ParsingAnnotation(content: String, failure: TracedFailure) extends Exception

object Parse {

  type Result[T] = Either[ParsingAnnotation, T]

  def parseResult[T](content: String, parser: P[_] => P[T]): Result[T] = {
    val parsed = fastparse.parse(content, parser)
    parsed match {
      case Success(value, index) => value.asRight
      case f: Failure            =>
        val traced = f.trace()
        scribe.error(s"failed to parse ${traced.longMsg}")
        ParsingAnnotation(content, traced).asLeft
    }
  }

  def document(blockContent: String): Result[Seq[Block]] =
    parseResult(blockContent, scitzen.parser.DocumentParsers.document(_))

  def paragraph(paragraphString: String): Result[Seq[InlineProv]] =
    parseResult(paragraphString, scitzen.parser.InlineParsers.fullParagraph(_))
}

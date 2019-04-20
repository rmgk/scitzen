package scitzen.parser

import cats.implicits._
import fastparse.P
import fastparse.Parsed.{Failure, Success}

case class ParsingAnnotation(content: String, failure: Failure) extends Exception

object Parse {

  type Result[T] = Either[ParsingAnnotation, T]

  def parseResult[T](content: String, parser: P[_] => P[T]): Result[T] = {
    val parsed = fastparse.parse(content, parser)
    parsed match {
      case Success(value, index) => value.asRight
      case f: Failure            =>
        scribe.error(s"failed to parse ${f.trace().longMsg}")
        ParsingAnnotation(content, f).asLeft
    }
  }

  def document(blockContent: String): Result[Document] =
    parseResult(blockContent, scitzen.parser.DocumentParsers.document(_))

  def paragraph(paragraphString: String): Result[Seq[Inline]] =
    parseResult(paragraphString, scitzen.parser.InlineParsers.fullParagraph(_))
}

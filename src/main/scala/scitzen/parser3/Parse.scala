package scitzen.parser3

import scitzen.parser.BlockParsers
import scitzen.sast.{Inline, Prov, Sast}
import cats.parse.Parser

case class ParsingAnnotation(content: String, failure: Parser.Error) extends Exception

object Parse {

  type Result[T] = Either[ParsingAnnotation, T]

  def parseResult[T](content: String, parser: Parser[T], prov: Prov): Result[T] = {
    val parsed = CommonParsers.providenceOffset.withValue(prov) { parser.parseAll(content) }
    parsed.left.map { err =>
      ParsingAnnotation(content, err)
    }
  }

  val parserDocument: Parser[Seq[Sast]] = (scitzen.parser3.BlockParsers.alternatives.rep.map(_.toList) <* Parser.end)

  def documentUnwrap(blockContent: String, prov: Prov): Seq[Sast] = {
    parseResult(blockContent, parserDocument, prov).toTry.get
  }

  def inlineUnwrap(paragraphString: String, prov: Prov): Seq[Inline] = {
    parseResult(paragraphString, InlineParsers.full(Parser.end, allowEmpty = false), prov).toTry.get._1
  }
}

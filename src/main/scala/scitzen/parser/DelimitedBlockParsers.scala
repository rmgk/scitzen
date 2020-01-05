package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object DelimitedBlockParsers {
  // we currently use ` for code, . for literal text,
  // = for nested content
  def anyStart[_: P]: P[String] = P(CharIn(".=`").rep(3).!)

  def makeDelimited[_: P](start: => P[String]): P[NormalBlock] =
    (start ~ AttributesParserOld.list.? ~ spaceLine ~/ Pass)
    .flatMap { case (delimiter, attr) =>
      def closing = eol ~ delimiter ~ spaceLine

      (withProv(untilE(closing, min = 0)) ~ closing)
      .map { case (content, prov) =>
        NormalBlock(delimiter, content, prov, attr.getOrElse(Nil))
      }
    }

  def full[_: P]: P[NormalBlock] = P(makeDelimited(anyStart))


  def whitespaceLiteral[_: P]: P[NormalBlock] = P(withProv(
    (significantVerticalSpaces.! ~ !newline).flatMap { indentation =>
      untilI(eol)
      .rep(min = 1, sep = significantSpaceLine.rep ~ indentation).!
      .map(lines => NormalBlock(indentation, lines.indent(-indentation.size), Prov(), Nil))
    }).map { case (nb, prov) => nb.copy(cprov = prov.copy(indent = nb.delimiter.length)) })

}

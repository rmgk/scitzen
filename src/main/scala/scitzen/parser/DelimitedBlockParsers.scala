package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object DelimitedBlockParsers {
  // we currently use ` for code, . for literal text,
  // = for nested asciidoc content
  def normalDelimiter[_ : P]: P[Unit] = CharIn(".=`").rep(3)
  def normalStart[_: P] = P(normalDelimiter)
  def anyStart[_: P]: P[String] = P(normalStart.! ~ spaceLine)

  def makeDelimited[_: P](start: => P[String]): P[NormalBlock] =
    (start ~/ Pass).flatMap { delimiter =>
      untilI(eol ~ delimiter ~ spaceLine, min = 0).map(content => NormalBlock(delimiter, content))
    }

  def full[_: P]: P[NormalBlock] = P(makeDelimited(anyStart))


  def whitespaceLiteral[_: P]: P[NormalBlock] = P(
    (significantVerticalSpaces.! ~ !newline).flatMap { indentation =>
      (untilI(eol) ~ significantSpaceLine.rep).!.rep(min = 1, sep = indentation)
                                              .map(lines => NormalBlock(indentation, lines.mkString))
    }
  )

}

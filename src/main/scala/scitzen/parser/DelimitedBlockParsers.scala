package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object DelimitedBlockParsers {
  val normalDelimitersChar: Seq[Char] = "=-.+_*".toSeq
  def normalDelimiter[_ : P]: P[Unit] = CharIn("=\\-.+_*").rep(4)
  def normalStart[_: P] = P(normalDelimiter)
  def anyStart[_: P]: P[String] = P((normalStart | "--" | "```" | ("|" ~ "=".rep(3))).! ~ iwsLine).log

  def makeDelimited[_: P](start: => P[String]): P[NormalBlock] =
    (start ~/ Pass).flatMap { delimiter =>
      untilI(eol ~ delimiter ~ iwsLine, min = 0).map(content => NormalBlock(BlockType.Delimited(delimiter), content))
    }

  def full[_: P]: P[NormalBlock] = P(makeDelimited(anyStart)).log


  def whitespaceLiteral[_: P]: P[NormalBlock] = P(
    (sws.! ~ !newline).flatMap { indentation =>
      (untilI(eol) ~ swsLine.rep).!.rep(min = 1, sep = indentation)
                                 .map(lines => NormalBlock(BlockType.Delimited(indentation), lines.mkString))
    }
  ).log

}

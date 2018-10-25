package asciimedic

import asciimedic.CommonParsers._
import fastparse._; import fastparse.NoWhitespace._

object DelimitedBlockParsers {
  val normalDelimiters: Seq[Char]  = "=-.+_*".toSeq
  def normalStart              [_:P]= P(normalDelimiters.map(c => P(c.toString.rep(4))).reduce(_ | _))
  def anyStart[_:P]: P[String] = P((normalStart | "--" | "```" | ("|" ~ "=".rep(3))).! ~ iwsLine)

  def makeDelimited[_:P](start: P[String]): P[NormalBlock] =
    (start ~/ Pass).flatMap { delimiter =>
      untilI(eol ~ delimiter ~ iwsLine, min = 0).map(content => NormalBlock(BlockType.Delimited(delimiter), content))
    }

  def full[_:P]: P[NormalBlock] = P(makeDelimited(anyStart))


  def whitespaceLiteral[_:P]: P[NormalBlock] = P(
    (sws.! ~ !newline).flatMap { indentation =>
      (untilI(eol) ~ swsLine.rep).!.rep(min = 1, sep = indentation)
      .map(lines => NormalBlock(BlockType.Delimited(indentation), lines.mkString))
    }
  ).log

}

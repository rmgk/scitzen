package asciimedic

import asciimedic.CommonParsers._
import fastparse.all._

object DelimitedBlockParsers {
  val normalDelimiters         = "=-.+_*"
  val normalStart              = P(normalDelimiters.map(c => c.toString.rep(4)).reduce(_ | _))
  val anyStart: Parser[String] = P((normalStart | "--" | "```" | ("|" ~ "=".rep(3))).! ~ iwsLine)

  def makeDelimited(start: Parser[String]): Parser[NormalBlock] =
    (start ~/ Pass).flatMap { delimiter =>
      untilI(eol ~ delimiter ~ iwsLine, min = 0).map(content => NormalBlock(BlockType.Delimited(delimiter), content))
    }

  val full: Parser[NormalBlock] = P(makeDelimited(anyStart))


  val whitespaceLiteral: Parser[NormalBlock] = P(
    (sws.! ~ !newline).flatMap { indentation =>
      (untilI(eol) ~ swsLine.rep).!.rep(min = 1, sep = indentation)
      .map(lines => NormalBlock(BlockType.Delimited(indentation), lines.mkString))
    }
  ).log()

}

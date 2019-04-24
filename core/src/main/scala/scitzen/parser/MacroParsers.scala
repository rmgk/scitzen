package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object MacroParsers {
  // ensure consistency between detect start and start
  def detectStart[_ : P]: P[Unit] = P(identifier ~ "[")
  def start[_: P]: P[String] = P(":" ~ identifier.! ~ &("["))

  def full[_: P]: P[Macro] = P(start ~ Attributes.list).map { (Macro.apply _).tupled }

}

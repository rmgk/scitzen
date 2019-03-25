package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object MacroParsers {
  def target[_: P]: P[String] = P(untilE("[" | significantAnySpaces, 0))
  def start[_: P]: P[String] = P(identifier.! ~ ":")
  def block[_: P]: P[BlockMacro] = P(start ~ ":" ~ !significantAnySpaces ~/ target ~ Attributes.list)
                                   .map {BlockMacro.fromTuple}
  def inline[_: P]: P[InlineMacro] = P(start ~ !significantAnySpaces ~ target ~ Attributes.list)
                                     .map {(InlineMacro.apply _).tupled}

}

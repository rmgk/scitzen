package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object MacroParsers {
  def target[_: P]: P[String] = P(untilE("[" | significantAnySpaces, 0))
  def start[_: P]: P[String] = P(identifier.! ~ ":")
  def block[_: P]: P[Macro] = P(start ~ ":" ~ !significantAnySpaces ~/ target ~ Attributes.list)
                              .map {(Macro.apply _).tupled}
  def inline[_: P]: P[Macro] = P(Index ~ start ~ !significantAnySpaces ~ target ~ Attributes.list ~ Index)
                                     .map {case (start, name, target, attributes, end) =>
                                       implicitly[P[_]].input
                                       Macro(name, target, attributes)
                                     }

}

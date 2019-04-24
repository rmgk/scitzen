package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object MacroParsers {
  // ensure consistency between detect start and start
  def detectStart[_ : P]: P[Unit] = P(identifier ~ "[")
  def start[_: P]: P[String] = P(":" ~ identifier.! ~ &("["))

  def target[_: P]: P[String] = P(untilE("[" | significantAnySpaces, 0))
  def block[_: P]: P[Macro] = P(identifier.! ~ "::" ~ !significantAnySpaces ~/ target ~ Attributes.list)
                              .map {(Macro.apply _).tupled}
  def block2[_: P]: P[Macro] = P(start ~ Attributes.list)
                              .map {case (id, attrs) => Macro.apply(id, "", attrs)}
  def inline[_: P]: P[Macro] = P(Index ~ start ~ !significantAnySpaces ~ target ~ Attributes.list ~ Index)
                                     .map {case (start, name, target, attributes, end) =>
                                       implicitly[P[_]].input
                                       Macro(name, target, attributes)
                                     }

}

package scitzen.parser
import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object MacroParsers {
  def target                      [_:P]= P(untilE("[" | significantAnySpaces))
  def start                       [_:P]= P(identifier.! ~ ":")
  def block [_:P]: P[BlockMacro]  = P(start ~ ":" ~ !significantAnySpaces ~/ target ~ Attributes.list)
                                    .map {(BlockMacro.apply _).tupled}
  def inline[_:P]: P[InlineMacro] = P(start ~ !significantAnySpaces ~ target ~ Attributes.list)
                                    .map {(InlineMacro.apply _).tupled}

}

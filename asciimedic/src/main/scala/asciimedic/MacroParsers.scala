package asciimedic

import asciimedic.CommonParsers._
import fastparse._; import fastparse.NoWhitespace._

object MacroParsers {
  def target                      [_:P]= P(untilE("[" | saws))
  def start                       [_:P]= P(identifier.! ~ ":")
  def block [_:P]: P[BlockMacro]  = P(start ~ ":" ~ !saws ~/ target ~ Attributes.list)
                                    .map {(BlockMacro.apply _).tupled}
  def inline[_:P]: P[InlineMacro] = P(start ~ !saws ~ target ~ Attributes.list)
                                    .map {(InlineMacro.apply _).tupled}


  /** urls are constrained macros, i.e., they may only start after a word boundary (all whitespace?) */
  object urls {
    def scheme [_:P]= P("http" ~ "s".? | "ftp" | "irc" | "mailto")

    def url[_:P]: P[InlineMacro] = P(scheme.! ~ ":" ~/ Attributes.value ~ Attributes.list.?)
                                   .map { case (s, t, a) => InlineMacro("link", s"$s:$t", a.getOrElse(Nil)) }
  }

}

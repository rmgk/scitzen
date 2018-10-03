package asciimedic

import asciimedic.CommonParsers._
import fastparse.all._

object MacroParsers {
  val target                      = P(untilE("[" | saws))
  val start                       = P(identifier.! ~ ":")
  val block : Parser[BlockMacro]  = P(start ~ ":" ~ !saws ~/ target ~ Attributes.list)
                                    .map {(BlockMacro.apply _).tupled}
  val inline: Parser[InlineMacro] = P(start ~ !saws ~ target ~ Attributes.list)
                                    .map {(InlineMacro.apply _).tupled}


  /** urls are constrained macros, i.e., they may only start after a word boundary (all whitespace?) */
  object urls {
    val scheme = P("http" ~ "s".? | "ftp" | "irc" | "mailto")

    val url: Parser[InlineMacro] = P(scheme.! ~ ":" ~/ Attributes.value ~ Attributes.list.?)
                                   .map { case (s, t, a) => InlineMacro("link", s"$s:$t", a.getOrElse(Nil)) }
  }

}

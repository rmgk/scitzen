package scitzen.parser

import de.rmgk.scip.*
import scitzen.sast.{Inline, InlineText}

import scala.annotation.tailrec

object InlineParsers {

  inline def directiveStart: Scip[Boolean] = ":".all
  inline def endingChars: Scip[Boolean]    = ":;\"]\n}".any

  inline def full(
      inline ending: Scip[Boolean]
  ): Scip[List[Inline]] = Scip {

    var start = scx.index

    @tailrec
    def inlineOptions(acc: List[Inline]): List[Inline] = {

      // quickly discard everything assumed to be unimportant
      // be aware that this limits what `ending` may start with
      until(endingChars).run

      val current = scx.index

      // closes the current run of plain text and adds it as a separate Inline node
      def addPlain(): List[Inline] =
        try
          if start < current
          then InlineText(scx.str(start, current)) :: acc
          else acc
        finally start = scx.index

      if ending.run then addPlain()
      else
        (DirectiveParsers.comment | DirectiveParsers.raw | DirectiveParsers.full).opt.run match
          case Some(inl) => inlineOptions(inl :: addPlain())
          case None =>
            if scx.next then inlineOptions(acc)
            else addPlain()
    }

    inlineOptions(Nil).reverse
  }

}

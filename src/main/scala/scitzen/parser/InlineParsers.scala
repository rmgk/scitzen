package scitzen.parser

import de.rmgk.scip.*
import scitzen.sast.{Inline, InlineText, Text}

import scala.annotation.{nowarn, tailrec}

object InlineParsers {

  inline def directiveStart: Scip[Boolean] = ":".all
  inline def endingChars: Scip[Boolean]    = ":;\"]\n}".any

  inline def full(
      inline ending: Scip[Boolean]
  ): Scip[Text] = Scip {

    val start = scx.index
    // start of the current text segment
    var textStart       = scx.index
    var beforePrevMatch = scx.index

    @tailrec
    def inlineOptions(acc: List[Inline]): List[Inline] = {

      // closes the current run of plain text and adds it as a separate Inline node
      def addPlain(current: Int): List[Inline] =
        try
          if textStart < current
          then InlineText(scx.str(textStart, current)) :: acc
          else acc
        finally textStart = scx.index

      // quickly discard everything assumed to be unimportant
      // be aware that this limits what `ending` may start with
      until(endingChars).run: @nowarn

      beforePrevMatch = scx.index
      if ending.run
      then addPlain(beforePrevMatch)
      else
        (DirectiveParsers.comment | DirectiveParsers.raw | DirectiveParsers.full).opt.run match
          case Some(inl) => inlineOptions(inl :: addPlain(beforePrevMatch))
          case None =>
            if scx.next then inlineOptions(acc)
            else addPlain(beforePrevMatch)
    }

    val inl = inlineOptions(Nil).reverse
    val raw = scx.str(start, beforePrevMatch)
    Text(inl, raw)

  }

}

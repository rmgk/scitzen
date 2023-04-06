package scitzen.parser

import de.rmgk.scip.*
import scitzen.parser.CommonParsers.*
import scitzen.parser.DirectiveParsers.{commentStart, syntaxStart}
import scitzen.sast.DCommand.Comment
import scitzen.sast.{Attribute, Directive, Inline, InlineText}

import scala.annotation.tailrec

object InlineParsers {

  def full(
      ending: Scip[Boolean],
      allowEmpty: Boolean = false
  ): Scip[List[Inline]] = Scip {

    var start = scx.index

    @tailrec
    def inlineOptions(acc: List[Inline]): List[Inline] = {
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
        (DirectiveParsers.commentEnding(ending) | DirectiveParsers.full).opt.run match
          case Some(inl) => inlineOptions(inl :: addPlain())
          case None =>
            if scx.next then inlineOptions(acc)
            else addPlain()
    }

    val inlines = inlineOptions(Nil).reverse

    if !allowEmpty && inlines.isEmpty then scx.fail else inlines

  }

}

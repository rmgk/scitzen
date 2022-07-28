package scitzen.parser

import de.rmgk.scip.*
import scitzen.sast.DCommand.Comment
import scitzen.sast.{Attribute, Directive, Inline, InlineText}
import scitzen.parser.CommonParsers.*
import scitzen.parser.DirectiveParsers.{commentStart, syntaxStart}

import scala.annotation.tailrec

object InlineParsers {

  def inlineAlternatives[T](
      ending: Scip[Boolean],
      alternatives: Scip[T],
      plain: String => T
  ): Scip[List[T]] =
    Scip {
      var start = scx.index

      @tailrec
      def inlineOptions(acc: List[T]): List[T] = {
        val current = scx.index

        def addPlain: List[T] =
          try
            if start < current
            then plain(scx.str(start, current)) :: acc
            else acc
          finally start = scx.index

        if ending.run then addPlain
        else
          alternatives.opt.run match
            case Some(inl) => inlineOptions(inl :: addPlain)
            case None =>
              if scx.next then inlineOptions(acc)
              else addPlain
      }

      inlineOptions(Nil).reverse

    }

  def full(
      endingFun: Scip[Boolean],
      allowEmpty: Boolean = false
  ): Scip[List[Inline]] = Scip {

    val inlines = inlineAlternatives(
      endingFun,
      DirectiveParsers.commentEnding(endingFun) | DirectiveParsers.full,
      InlineText.apply
    ).run
    if !allowEmpty && inlines.isEmpty then scx.fail else inlines

  }

}

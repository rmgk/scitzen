package scitzen.parser

import de.rmgk.scip.*
import scitzen.sast.DCommand.Comment
import scitzen.sast.{Attribute, Directive, Inline, InlineText}
import scitzen.parser.CommonParsers.*
import scitzen.parser.DirectiveParsers.commentStart

object InlineParsers {

  def full(
      endChars: Scip[Boolean],
      endingFun: Scip[Boolean],
      allowEmpty: Boolean = false
  ): Scip[List[Inline]] = {

    val comment: Scip[Directive] =
      withProv(commentStart ifso (until(eolB).min(0) and (endingFun.lookahead or eolB)).str)
        .map { case (text, prov) => Directive(Comment, Attribute("", text).toAttributes)(prov) }

    val notSyntax: Scip[InlineText] = Scip {
      val start = scx.index
      while
        val start = scx.index
        (until(":".any or endChars).min(0).trace("until end") and (
          (DirectiveParsers.syntaxStart.lookahead or ":".all)
          or (endingFun.lookahead.trace("ending fun") or endChars)
        )).orFail.run
        scx.index > start
      do ()
      if start == scx.index then scx.fail
    }.dropstr.map(InlineText.apply)

    val inlineSequence: Scip[List[Inline]] =
      (comment.trace("comment")
        | DirectiveParsers.full.trace("directive")
        | notSyntax.trace("not syntax")).list(Scip {
        true
      }).require {
        _.nonEmpty || allowEmpty
      }.trace("inlines full")

    inlineSequence <~ endingFun.orFail

  }

}

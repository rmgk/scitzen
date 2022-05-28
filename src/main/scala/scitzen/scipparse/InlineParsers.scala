package scitzen.scipparse

import de.rmgk.scip.*
import scitzen.sast.DCommand.Comment
import scitzen.sast.{Attribute, Directive, Inline, InlineText}
import scitzen.scipparse.CommonParsers.*
import scitzen.scipparse.DirectiveParsers.commentStart

object InlineParsers {

  def full(
      endChars: Scip[Boolean],
      endingFun: Scip[Boolean],
      allowEmpty: Boolean = false
  ): Scip[List[Inline]] = {

    val comment: Scip[Directive] =
      withProv(commentStart ifso (until(eolB).min(0) and (endingFun.lookahead or eolB)).str)
        .map { case (text, prov) => Directive(Comment, Attribute("", text).toAttributes)(prov) }.trace("comment")

    val notSyntax: Scip[String] = Scip {
      val start = scx.index
      while
        val start = scx.index
        (until(":".any.or(endChars)).min(0) and (
          (DirectiveParsers.syntaxStart.lookahead or ":".all)
          or (endingFun.lookahead or endChars)
        )).orFail.run
        scx.index > start
      do ()
      if start == scx.index then scx.fail
    }.dropstr.trace("plaintext")

    val simpleText: Scip[InlineText] = {
      notSyntax.map(InlineText.apply)
    }

    val inlineSequence: Scip[List[Inline]] =
      choice(comment, DirectiveParsers.full.trace("directive"), simpleText).list(Scip { true }).require {
        _.nonEmpty || allowEmpty
      }.trace("inlines full")

    inlineSequence <~ endingFun.orFail

  }

}

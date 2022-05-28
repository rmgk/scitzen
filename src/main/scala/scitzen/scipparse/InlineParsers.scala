package scitzen.scipparse

import scitzen.sast.{Attribute, Inline, InlineText, Directive}
import de.rmgk.scip.*
import CommonParsers.*
import DirectiveParsers.commentStart
import scitzen.sast.DCommand.Comment
import CompatParsers.*

case class InlineParsers(endChars: Scip[Boolean], endingFun: Scip[Boolean], allowEmpty: Boolean = false) {

  val comment: Scip[Directive] =
    withProv(commentStart ifso (until(eolB).min(0) and (endingFun.lookahead or eolB)).str)
      .map { case (text, prov) => Directive(Comment, Attribute("", text).toAttributes)(prov) }.trace("comment")

  private val notSyntax: Scip[String] = Scip {
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

  val inlineSequence: Scip[Seq[Inline]] =
    choice(comment, DirectiveParsers.full.trace("directive"), simpleText).list(Scip { true }).require {
      _.nonEmpty || allowEmpty
    }

  val full: Scip[(Seq[Inline], String)] =
    Scip((inlineSequence.trace("inlines full").run, endingFun.str.trace("requester end").run))

}

package scitzen.scipparse

import scitzen.sast.{Attribute, Inline, InlineText, Directive}
import de.rmgk.scip.*
import CommonParsers.*
import DirectiveParsers.commentStart
import scitzen.sast.DCommand.Comment
import CompatParsers.*

case class InlineParsers(endChars: Scip[Boolean], endingFun: Scip[Unit], allowEmpty: Boolean = false) {

  val comment: Scip[Directive] =
    withProv(commentStart ~> (untilE(eol, min = 0) ~ choice(endingFun.lookahead, eol)).str)
      .map { case (text, prov) => Directive(Comment, Attribute("", text).toAttributes)(prov) }.trace("comment")

  private val notSyntax: Scip[String] = Scip {
    val start = scx.index
    while
      val start = scx.index
      until(":".any.or(endChars)).run
      DirectiveParsers.syntaxStart.attempt.lookahead.or(":".scip.attempt).or(
        endingFun.attempt.lookahead.or(endChars)
      ).falseFail("").run
      scx.index > start
    do ()
    if start == scx.index then scx.fail("")
  }.str.trace("plaintext")

  val simpleText: Scip[InlineText] = {
    notSyntax.map(InlineText.apply)
  }

  val inlineSequence: Scip[Seq[Inline]] =
    choice(comment, DirectiveParsers.full.trace("directive"), simpleText).list(Scip {}).require {
      _.nonEmpty || allowEmpty
    }

  val full: Scip[(Seq[Inline], String)] =
    Scip((inlineSequence.trace("inlines full").run, endingFun.str.trace("requester end").run))

}

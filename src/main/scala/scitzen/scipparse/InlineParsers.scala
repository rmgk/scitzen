package scitzen.scipparse

import scitzen.sast.{Attribute, Inline, InlineText, Directive}
import de.rmgk.scip.*
import CommonParsers.*
import DirectiveParsers.commentStart
import scitzen.sast.DCommand.Comment
import CompatParsers.*

case class InlineParsers(endChars: Scip[Boolean], endingFun: Scip[Unit], allowEmpty: Boolean = false) {

  val comment: Scip[Directive] =
    withProv(commentStart.orFail ~> (untilE(eol, min = 0) <~ choice(endingFun.lookahead, eol)).str)
      .map { case (text, prov) => Directive(Comment, Attribute("", text).toAttributes)(prov) }.trace("comment")

  private val notSyntax: Scip[String] = Scip {
    val start = scx.index
    while
      val start = scx.index
      until(":".any.or(endChars)).run
      DirectiveParsers.syntaxStart.lookahead.trace(s"syntax start").or(":".scip.trace(s"colon")).or(
        endingFun.attempt.trace(s"attempted endingfun").lookahead.or(endChars.trace(s"end chars"))
      ).orFail.run
      scx.index > start
    do ()
    if start == scx.index then scx.fail("")
  }.str.trace("plaintext")

  val simpleText: Scip[InlineText] = {
    notSyntax.map(InlineText.apply)
  }

  val inlineSequence: Scip[Seq[Inline]] =
    choice(comment, DirectiveParsers.full.trace("directive"), simpleText).list(Scip {true}).require {
      _.nonEmpty || allowEmpty
    }

  val full: Scip[(Seq[Inline], String)] =
    Scip((inlineSequence.trace("inlines full").run, endingFun.str.trace("requester end").run))

}

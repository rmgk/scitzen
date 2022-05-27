package scitzen.scipparse

import scitzen.sast.{Attribute, Inline, InlineText, Directive}
import de.rmgk.scip.*
import CommonParsers.*
import DirectiveParsers.commentStart
import scitzen.sast.DCommand.Comment
import CompatParsers.*

case class InlineParsers(endChars: String, endingFun: Scip[Unit], allowEmpty: Boolean = false) {

  def ending: Scip[Unit] = endingFun

  def comment: Scip[Directive] =
    withProv(Scip { commentStart.run; (untilE(eol, min = 0) ~ choice(ending.lookahead, eol)).str.run })
      .map { case (text, prov) => Directive(Comment, Attribute("", text).toAttributes)(prov) }.trace("comment")

  private def notSyntax: Scip[String] =
    choice(
      CharsWhile(c => c != ':' && !endChars.contains(c), 1).trace("chars"),
      (DirectiveParsers.syntaxStart.attempt.map(!_).falseFail("") ~ ":".scip).trace("dirs"),
      (ending.attempt.map(!_).falseFail("") ~ CharPred(endChars.contains(_))).trace("ends")
    ).trace("not syntax choice").attempt.rep.require(_ >= 1).drop.str.trace("outer rep")

  def simpleText: Scip[InlineText] = {
    notSyntax.map(InlineText.apply)
  }

  val inlineSequence: Scip[Seq[Inline]] =
    choice(comment, DirectiveParsers.full.trace("directive"), simpleText).list(Scip {}).require {
      _.nonEmpty || allowEmpty
    }

  def full: Scip[(Seq[Inline], String)] =
    Scip((inlineSequence.run, ending.str.run)).trace("full")

}

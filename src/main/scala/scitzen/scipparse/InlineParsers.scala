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
    withProv(Scip { commentStart.run; (untilE(eol, min = 0) ~ choice(ending.lookahead, eol)).!.run })
      .map { case (text, prov) => Directive(Comment, Attribute("", text).toAttributes)(prov) }.named("comment")

  private def notSyntax: Scip[String] =
    choice(
      CharsWhile(c => c != ':' && !endChars.contains(c), 1).named("chars"),
      (DirectiveParsers.syntaxStart.?.map(!_).falseFail("") ~ ":".scip).named("dirs"),
      (ending.?.map(!_).falseFail("") ~ CharPred(endChars.contains(_))).named("ends")
    ).named("not syntax choice").rep(1).!.named("outer rep")

  def simpleText: Scip[InlineText] = {
    notSyntax.map(InlineText.apply)
  }

  def inlineSequence: Scip[Seq[Inline]] =
    repeat(choice(comment, DirectiveParsers.full.named("directive"), simpleText), Scip {}, if (allowEmpty) 0 else 1)

  def full: Scip[(Seq[Inline], String)] =
    Scip((inlineSequence.run, ending.!.run)).named("full")

}

package scitzen.parser

import de.rmgk.scip.*
import scitzen.sast.DCommand.Comment
import scitzen.sast.{Attribute, Directive, Inline, InlineText}
import scitzen.parser.CommonParsers.*
import scitzen.parser.DirectiveParsers.{commentStart, syntaxStart}

object InlineParsers {

  def full(
      endingFun: Scip[Boolean],
      allowEmpty: Boolean = false
  ): Scip[List[Inline]] = {

    val comment: Scip[Directive] =
      withProv(commentStart ifso (until(eolB).min(0) and (endingFun.lookahead or eolB)).str)
        .map { case (text, prov) => Directive(Comment, Attribute("", text).toAttributes)(prov) }

    val notSyntax: Scip[InlineText] =
      until(syntaxStart or endingFun).min(1).orFail.dropstr.map(InlineText.apply)

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

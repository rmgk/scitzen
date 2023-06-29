package scitzen.parser

import scitzen.parser.CommonParsers.*
import scitzen.sast.DCommand.Comment
import scitzen.sast.{Attribute, Attributes, DCommand, Directive, InlineText}
import de.rmgk.scip.*
import InlineParsers.directiveStart

object DirectiveParsers {

  val detectStart: Scip[Boolean] = directiveStart and Identifier.identifier.opt and AttributesParser.open.all
  val macroCommand: Scip[String] = identifierB.str

  val full: Scip[Directive] = withProv(Scip {
    directiveStart.orFail.run
    (macroCommand.opt.run, AttributesParser.braces.run)
  }).map {
    case ((name, attributes), prov) =>
      Directive(DCommand.parseMacroCommand(name.getOrElse("")), Attributes(attributes))(prov)
  }

  val raw: Scip[InlineText] = Scip {
    directiveStart.orFail.run
    val open = ("\"".all.rep.min(1).str <~ "[".all.orFail).run
    val len  = open.length
    val str  = untilIS("]".all and "\"".all.rep.min(len)).run
    InlineText(str, len)
  }

  inline def commentStart: Scip[Boolean] = ":%".all

  val syntaxStart: Scip[Boolean] = commentStart or DirectiveParsers.detectStart

  inline def commentEnding(inline endingFun: Scip[Boolean]): Scip[Directive] =
    withProv(commentStart ifso (until(eol).min(0) and (endingFun.lookahead or eol)).str)
      .map { case (text, prov) => Directive(Comment, Attribute("", text).toAttributes)(prov) }

  val comment: Scip[Directive] = commentEnding(Scip { false })

}

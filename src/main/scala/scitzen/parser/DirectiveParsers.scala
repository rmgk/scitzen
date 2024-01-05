package scitzen.parser

import de.rmgk.scip.*
import scitzen.parser.CommonParsers.*
import scitzen.parser.InlineParsers.directiveStart
import scitzen.sast.DCommand.Comment
import scitzen.sast.{Attribute, Attributes, DCommand, Directive, InlineText}

object DirectiveParsers {

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

  val comment: Scip[Directive] =
    withProv(commentContent.str)
      .map { case (text, prov) => Directive(Comment, Attribute("", text).toAttributes)(prov) }

  val commentContent: Scip[Boolean] = commentStart and until(eol).min(0) and eol

}

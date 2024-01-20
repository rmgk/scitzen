package scitzen.parser

import de.rmgk.scip.*
import scitzen.parser.CommonParsers.*
import scitzen.parser.InlineParsers.directiveStart
import scitzen.sast.DCommand.Comment
import scitzen.sast.{Attribute, Attributes, DCommand, Directive, InlineText, Meta, Prov}

/** Directives generally start with : and have a fixed ending.
  * If used as blocks, one needs to consider initial whitespace and the end of line, which are not considered part of the directive by these parsers
  */
object DirectiveParsers {

  val macroCommand: Scip[String] = identifierB.str

  val full: Scip[Directive] = Scip {
    val start = scx.index
    directiveStart.orFail.run
    val name       = macroCommand.opt.run
    val attributes = AttributesParser.braces.run
    Directive(DCommand.parseMacroCommand(name.getOrElse("")), Attributes(attributes), Meta("", Prov(start, scx.index)))
  }

  val raw: Scip[InlineText] = Scip {
    directiveStart.orFail.run
    val open = ("\"".all.rep.min(1).str <~ "[".all.orFail).run
    val len  = open.length
    val str  = untilIS("]".all and "\"".all.rep.min(len)).run
    InlineText(str, len)
  }

  inline def commentStart: Scip[Boolean] = ":%".all

  val comment: Scip[Directive] = Scip {
    val start = scx.index
    val text  = commentContent.run
    Directive(Comment, Attributes.target(text), Meta("", Prov(start, scx.index)))
  }

  val commentContent: Scip[String] = (commentStart ifso until(eol).min(0).str)

}

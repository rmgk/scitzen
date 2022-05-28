package scitzen.scipparse

import scitzen.scipparse.CommonParsers._
import scitzen.sast.DCommand.Comment
import scitzen.sast.{Attribute, Attributes, Directive, DCommand}
import de.rmgk.scip.*

object DirectiveParsers {
  val detectStart: Scip[Boolean] = (":".all and (Identifier.identifier or Scip(true)) and AttributesParser.open.all)
  val macroCommand: Scip[String] = identifierB.str

  val full: Scip[Directive] = withProv(Scip {
    ":".all.orFail.run
    (macroCommand.opt.run, AttributesParser.braces.run)
  }).map {
    case ((name, attributes), prov) =>
      Directive(DCommand.parseMacroCommand(name.getOrElse("")), Attributes(attributes))(prov)
  }

  inline def commentStart: Scip[Boolean] = ":%".all

  val syntaxStart: Scip[Boolean] = commentStart or DirectiveParsers.detectStart

  val comment: Scip[Directive] =
    (withProv(Scip { commentStart.orFail.run; untilI(eolB).dropstr.run }))
      .map { case (text, prov) => Directive(Comment, Attribute("", text).toAttributes)(prov) }

}
package scitzen.scipparse

import scitzen.scipparse.CommonParsers._
import scitzen.sast.DCommand.Comment
import scitzen.sast.{Attribute, Attributes, Directive, DCommand}
import de.rmgk.scip.*

object DirectiveParsers {
  val detectStart: Scip[Boolean] = (":".scip and Identifier.identifier and AttributesParser.open.scip)
  val macroCommand: Scip[String] = identifier.str

  val full: Scip[Directive] = withProv(Scip {
    ":".scip.orFail.run
    (macroCommand.opt.run, AttributesParser.braces.run)
  }).map {
    case ((name, attributes), prov) =>
      Directive(DCommand.parseMacroCommand(name.getOrElse("")), Attributes(attributes))(prov)
  }

  val commentStart: Scip[Boolean] = ":%".scip

  val syntaxStart: Scip[Boolean] = commentStart or DirectiveParsers.detectStart

  val comment: Scip[Directive] =
    (withProv(Scip { commentStart.orFail.run; untilI(eol).map(_ => ()).str.run }))
      .map { case (text, prov) => Directive(Comment, Attribute("", text).toAttributes)(prov) }

}

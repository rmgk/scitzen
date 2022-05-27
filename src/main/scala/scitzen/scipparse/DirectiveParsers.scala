package scitzen.scipparse

import scitzen.scipparse.CommonParsers._
import scitzen.sast.DCommand.Comment
import scitzen.sast.{Attribute, Attributes, Directive, DCommand}
import de.rmgk.scip.*

object DirectiveParsers {
  val detectStart: Scip[Unit]    = (":".scip ~ identifier.? ~ AttributesParser.start)
  val macroCommand: Scip[String] = (identifier.!)

  val full: Scip[Directive] = withProv(Scip {
    ":".scip.run
    (macroCommand.opt.run, AttributesParser.braces.run)
  }).map {
    case ((name, attributes), prov) =>
      Directive(DCommand.parseMacroCommand(name.getOrElse("")), Attributes(attributes))(prov)
  }

  val commentStart: Scip[Unit] = ":%".scip

  val syntaxStart: Scip[Unit] = choice(commentStart, DirectiveParsers.detectStart)

  val comment: Scip[Directive] =
    (withProv(Scip { commentStart.run; untilI(eol).map(_ => ()).!.run }))
      .map { case (text, prov) => Directive(Comment, Attribute("", text).toAttributes)(prov) }

}

package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.sast.DCommand.Comment
import scitzen.sast.{Attribute, Attributes, Directive, DCommand}

object DirectiveParsers {
  def detectStart[_p: P]: P[Unit]    = P(":" ~ identifier.? ~ AttributesParser.start)
  def macroCommand[_p: P]: P[String] = P(identifier.!)

  def full[_p: P]: P[Directive] =
    P(withProv(":" ~ macroCommand.? ~ AttributesParser.braces)).map {
      case ((name, attributes), prov) =>
        Directive(DCommand.parseMacroCommand(name.getOrElse("")), Attributes(attributes))(prov)
    }

  def commentStart[_p: P]: P[Unit] = P(":%")

  def syntaxStart[_p: P]: P[Unit] = P(commentStart | DirectiveParsers.detectStart)

  def comment[_p: P]: P[Directive] =
    P(withProv(commentStart ~ untilI(eol).!))
      .map { case (text, prov) => Directive(Comment, Attribute("", text).toAttributes)(prov) }

}

package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.sast.MacroCommand.Comment
import scitzen.sast.{Attribute, Attributes, Macro, MacroCommand}

object MacroParsers {
  def detectStart[_p: P]: P[Unit]    = P(":" ~ identifier.? ~ AttributesParser.start)
  def macroCommand[_p: P]: P[String] = P(identifier.!)

  def full[_p: P]: P[Macro] =
    P(withProv(":" ~ macroCommand.? ~ AttributesParser.braces)).map {
      case ((name, attributes), prov) =>
        Macro(MacroCommand.parseMacroCommand(name.getOrElse("")), Attributes(attributes), prov)
    }

  def commentStart[_p: P]: P[Unit] = P(":%")

  def syntaxStart[_p: P]: P[Unit] = P(commentStart | MacroParsers.detectStart)

  def comment[_p: P]: P[Macro] =
    P(withProv(commentStart ~ untilI(eol).!))
      .map { case (text, prov) => Macro(Comment, Attribute("", text).toAttributes, prov) }

}

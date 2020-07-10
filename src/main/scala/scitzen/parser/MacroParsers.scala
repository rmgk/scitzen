package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.parser.MacroCommand.Comment

object MacroParsers {
  def detectStart[_: P]: P[Unit]    = P(":" ~ identifier.? ~ AttributesParser.start)
  def macroCommand[_: P]: P[String] = P(identifier.?.!)

  def full[_: P]: P[Macro] =
    P(withProv(":" ~ macroCommand ~ AttributesParser.braces)).map {
      case ((name, attributes), prov) => Macro(MacroCommand.parse(name), Attributes(attributes, prov))
    }

  def commentStart[_: P]: P[Unit] = P(":%")

  def syntaxStart[_: P]: P[Unit] = P(commentStart | MacroParsers.detectStart)

  def comment[_: P]: P[Macro] =
    P(withProv(commentStart ~ untilI(eol).!))
      .map { case (text, prov) => Macro(Comment, Attribute("", text).toAttributes(prov)) }

}

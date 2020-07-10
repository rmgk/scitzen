package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object MacroParsers {
  def detectStart[_: P]: P[Unit] = P(":" ~ identifier.? ~ AttributesParser.start)
  def macroCommand[_: P]: P[String] = P(identifier.?.!)

  def plain[_ : P]: P[Macro] = P(withProv(macroCommand ~ AttributesParser.braces)).map {
    case ((name, attributes), prov) => Macro(MacroCommand.parse(name), Attributes(attributes, prov))
  }

  def full[_: P]: P[Macro] = P(":" ~ plain)
}

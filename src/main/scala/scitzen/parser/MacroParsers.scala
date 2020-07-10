package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.parser.MacroCommand.Comment
import scitzen.parser.Sast.SMacro

object MacroParsers {
  def detectStart[_: P]: P[Unit]    = P(":" ~ identifier.? ~ AttributesParser.start)
  def macroCommand[_: P]: P[String] = P(identifier.?.!)

  def full[_: P]: P[SMacro] =
    P(withProv(":" ~ macroCommand ~ AttributesParser.braces)).map {
      case ((name, attributes), prov) => SMacro(MacroCommand.parse(name), Attributes(attributes, prov))
    }

  def commentStart[_: P]: P[Unit] = P(":%")

  def syntaxStart[_: P]: P[Unit] = P(commentStart | MacroParsers.detectStart)

  def comment[_: P]: P[SMacro] =
    P(withProv(commentStart ~ untilI(eol).!))
      .map { case (text, prov) => SMacro(Comment, Attribute("", text).toAttributes(prov)) }

}

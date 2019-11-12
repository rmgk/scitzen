package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.parser.MacroCommand.{Cite, Other, Quote}

object MacroParsers {
  // ensure consistency between detect start and start
  def detectStart[_: P]: P[Unit] = P(identifier.? ~ "[")
  def start[_: P]: P[String] = P(":" ~ identifier.?.! ~ &("["))

  def full[_: P]: P[Macro] = P(withProv(start ~ AttributesParser.list)).map {
    case ((name, attributes), prov) => Macro(MacroCommand.parse(name), Attributes.l(attributes, prov))
  }

  def texHack[_: P]: P[Inline] = P(withProv("\\" ~ identifier.! ~ "{" ~ CharsWhile(_ != '}').?.! ~ "}")
                                  .map{ case ((i, c), p) =>
                                      i match {
                                        case "sysname" => Macro(Other("n"), Attributes.a(Attribute("", "sysname"), p))
                                        case "fsysname" => Macro(Other("n"), Attributes.a(Attribute("", "fsysname"), p))
                                        case "basesysname" => Macro(Other("n"), Attributes.a(Attribute("", "sysname"), p))
                                        case "cite" => Macro(Cite, Attributes.a(Attribute("", c), p))
                                        case "citet" => Macro(Cite, Attributes.l(List(Attribute("style", "name"), Attribute("", c)), p))
                                        case arg@("subparagraph"|"todo"|"ref"|"caption"|"textsf"|"textsc"|"creation"|"footnote"|"label") =>
                                          Macro(Other(arg), Attributes.a(Attribute("", c), p))
                                        case "textit" => Macro(Quote("_"), Attributes.a(Attribute("", c), p))
                                        case "emph" => Macro(Quote("_"), Attributes.a(Attribute("", c), p))
                                        case "code" => Macro(Quote("`"), Attributes.a(Attribute("", c), p))
                                        case a @ ("begin"|"end"|"newcommand") =>  InlineText(s"\\$a{$c}")
                                      }
                                  })

}

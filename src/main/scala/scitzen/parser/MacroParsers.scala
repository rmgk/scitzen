package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.parser.MacroCommand.{Cite, Code, Emph, Other}

object MacroParsers {
  // ensure consistency between detect start and start
  def detectStart[_: P]: P[Unit] = P(identifier.? ~ AttributesParser.start)
  def macroCommand[_: P]: P[String] = P(identifier.?.!)

  def full[_: P]: P[Macro] = P(withProv(":" ~ macroCommand ~ AttributesParser.braces)).map {
    case ((name, attributes), prov) => Macro(MacroCommand.parse(name), Attributes(attributes, prov))
  }

  def texHack[_: P]: P[Inline] =
    P(withProv("\\" ~ identifier.! ~ "{" ~ CharsWhile(_ != '}').?.! ~ "}")
      .map { case ((i, c), p) =>
        i match {
          case "sysname"     => Macro(Other("n"), Attribute("", "sysname").toAttributes(p))
          case "fsysname"    => Macro(Other("n"), Attribute("", "fsysname").toAttributes(p))
          case "basesysname" => Macro(Other("n"), Attribute("", "sysname").toAttributes(p))
          case "citet"       => Macro(Cite, Attributes(List(Attribute("style", "name"), Attribute("", c)), p))
          case "textit"      => Macro(Emph, Attribute("", c).toAttributes(p))
          case "emph"        => Macro(Emph, Attribute("", c).toAttributes(p))
          case "code"        => Macro(Code, Attribute("", c).toAttributes(p))

          case a @ ("begin" | "end" | "newcommand") => InlineText(s"\\$a{$c}")

          case arg @ ("ref" | "label" | "cite" | "subparagraph" | "todo" | "caption" | "textsf" | "textsc" | "creation" | "footnote") =>
            Macro(MacroCommand.parse(arg), Attribute("", c).toAttributes(p))

        }
      })

}

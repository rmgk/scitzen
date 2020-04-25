package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.parser.MacroCommand.{Cite, Code, Emph, Other}

object MacroParsers {
  // ensure consistency between detect start and start
  def detectStart[_: P]: P[Unit] = P(identifier.? ~ AttributesParser.start)
  def start[_: P]: P[String] = P(":" ~ identifier.?.! ~ &(AttributesParser.start))

  def full[_: P]: P[Macro] = P(withProv(start ~ AttributesParser.list)).map {
    case ((name, attributes), prov) => Macro(MacroCommand.parse(name), Attributes(attributes, prov))
  }

  def texHack[_: P]: P[Inline] =
    P(withProv("\\" ~ identifier.! ~ "{" ~ CharsWhile(_ != '}').?.! ~ "}")
      .map { case ((i, c), p) =>
        i match {
          case "sysname"     => Macro(Other("n"), Attributes.a(Attribute("", "sysname"), p))
          case "fsysname"    => Macro(Other("n"), Attributes.a(Attribute("", "fsysname"), p))
          case "basesysname" => Macro(Other("n"), Attributes.a(Attribute("", "sysname"), p))
          case "citet"       => Macro(Cite, Attributes(List(Attribute("style", "name"), Attribute("", c)), p))
          case "textit"      => Macro(Emph, Attributes.a(Attribute("", c), p))
          case "emph"        => Macro(Emph, Attributes.a(Attribute("", c), p))
          case "code"        => Macro(Code, Attributes.a(Attribute("", c), p))

          case a @ ("begin" | "end" | "newcommand") => InlineText(s"\\$a{$c}")

          case arg @ ("ref" | "label" | "cite" | "subparagraph" | "todo" | "caption" | "textsf" | "textsc" | "creation" | "footnote") =>
            Macro(MacroCommand.parse(arg), Attributes.a(Attribute("", c), p))

        }
      })

}

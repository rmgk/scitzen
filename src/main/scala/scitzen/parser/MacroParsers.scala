package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.parser.MacroCommand.Comment
import scitzen.parser.Sast.Macro

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


sealed trait MacroCommand
object MacroCommand {
  val (parseMap, printMap) = {
    val standard = List(
      "cite"    -> Cite,
      "comment" -> Comment,
      "def"     -> Def,
      "image"   -> Image,
      "include" -> Include,
      "label"   -> Label,
      "link"    -> Link,
      "ref"     -> Ref,
      "code"    -> Code,
      "emph"    -> Emph,
      "strong"  -> Strong,
      "math"    -> Math,
      ""        -> Lookup
      )
    val aliases = Map(
      "fence" -> Include,
      "_"     -> Emph,
      "`"     -> Code,
      "*"     -> Strong,
      "$"     -> Math,
      "n"     -> Lookup
      )

    (standard.toMap ++ aliases, standard.map(p => p._2 -> p._1).toMap)
  }
  def parse(str: String): MacroCommand = parseMap.getOrElse(str, Other(str))
  def print(m: MacroCommand): String =
    m match {
      case Other(str) => str
      case o          => printMap(o)
    }

  object Code                   extends MacroCommand
  object Emph                   extends MacroCommand
  object Strong                 extends MacroCommand
  object Math                   extends MacroCommand
  object Cite                   extends MacroCommand
  object Comment                extends MacroCommand
  object Def                    extends MacroCommand
  object Image                  extends MacroCommand
  object Include                extends MacroCommand
  object Label                  extends MacroCommand
  object Link                   extends MacroCommand
  object Ref                    extends MacroCommand
  object Lookup                 extends MacroCommand
  case class Other(str: String) extends MacroCommand
}

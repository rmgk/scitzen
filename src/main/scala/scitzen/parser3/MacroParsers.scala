package scitzen.parser3

import cats.parse.{Numbers, Rfc5234, Parser as P, Parser0 as P0}
import cats.syntax.*
import cats.implicits.*
import cats.parse.Numbers.digits
import cats.parse.Rfc5234.sp
import cats.parse.Parser.*
import CommonParsers._
import scitzen.sast.MacroCommand.Comment
import scitzen.sast.{Attribute, Attributes, Macro, MacroCommand}

object MacroParsers {
  val detectStart: P[Unit]    = (":" ~ identifier.? ~ scitzen.parser3.AttributesParser.start).void
  val macroCommand: P[String] = (identifier.string)

  val full: P[Macro] =
    (withProv(":" *> macroCommand.? ~ scitzen.parser3.AttributesParser.braces)).map {
      case ((name, attributes), prov) =>
        Macro(MacroCommand.parseMacroCommand(name.getOrElse("")), Attributes(attributes), prov)
    }

  val commentStart: P[Unit] = (":%")

  val syntaxStart: P[Unit] = (commentStart | MacroParsers.detectStart)

  val comment: P[Macro] =
    (withProv(restOfLine(start = commentStart).map(_._2)))
      .map { case (text, prov) => Macro(Comment, Attribute("", text).toAttributes, prov) }

}

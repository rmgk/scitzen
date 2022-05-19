package scitzen.parser3

import cats.implicits.*
import cats.parse.Numbers.digits
import cats.parse.Parser.*
import cats.parse.Rfc5234.sp
import cats.parse.{Numbers, Rfc5234, Parser as P, Parser0 as P0}
import cats.syntax.*
import scitzen.parser3.CommonParsers.*
import scitzen.sast.DCommand.Comment
import scitzen.sast.{Attribute, Attributes, Directive, DCommand}

object MacroParsers {
  val macroCommand: P[String] = (identifier.string)

  val full: P[Directive] =
    (withProv(":" *> macroCommand.? ~ AttributesParser.braces)).map {
      case ((name, attributes), prov) =>
        Directive(DCommand.parseMacroCommand(name.getOrElse("")), Attributes(attributes))(prov)
    }.withContext("macro")

  val comment: P[Directive] =
    (withProv(restOfLine(start = commentStart).map(_._2)))
      .map { case (text, prov) => Directive(Comment, Attribute("", text).toAttributes)(prov) }

}

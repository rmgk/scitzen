package scitzen.parser3

import cats.parse.{Numbers, Rfc5234, Parser as P, Parser0 as P0}
import cats.syntax.*
import cats.implicits.*
import cats.parse.Numbers.digits
import cats.parse.Rfc5234.sp
import cats.parse.Parser.*
import CommonParsers._
import scitzen.parser.{AttributesParser, MacroParsers}
import scitzen.sast.{Attribute, Attributes, Block, Fenced, Prov, Sast}

object DelimitedBlockParsers {
  // use ` for verbatim text, : for parsed text
  val anyStart: P[String] = (charIn(":`").rep(2).string)

  def makeDelimited(start: P[String]): P[Block] =
    (start ~ scitzen.parser3.MacroParsers.macroCommand.? ~ scitzen.parser3.AttributesParser.braces.? <* spaceLine).flatMap {
      case ((delimiter, command), attr) =>
        (withProv(untilI(eol.with1 <* delimiter <* spaceLine)))
          .map {
            case (text, prov) =>
              val (isStripped, strippedText) = stripIfPossible(text, delimiter.length)
              val rawAttr                    = command.map(Attribute("", _)) ++: attr.getOrElse(Nil)
              val blockContent =
                delimiter(0) match {
                  case '`' => Fenced(strippedText)
                  case ':' =>
                    val sast: Seq[Sast] = scitzen.parser3.Parse.documentUnwrap(strippedText, prov)
                    scitzen.sast.Parsed(delimiter, sast)
                }
              scitzen.sast.Block(
                Attributes(rawAttr),
                blockContent,
                if (!isStripped) prov else prov.copy(indent = delimiter.length)
              )
          }
    }

  val anyDelimited: P[Block] = (makeDelimited(anyStart))

  val spaceNewline = " *\\n?$".r

  def stripIfPossible(str: String, i: Int): (Boolean, String) = {
    val prefix = " ".repeat(i)
    true -> str.linesWithSeparators.map { l =>
      if (l.startsWith(prefix)) l.substring(i)
      else if (spaceNewline.matches(l)) l
      else return (false, str)
    }.mkString
  }

  val whitespaceLiteral: P[Block] =
    (withProv(((index.with1 <* significantSpaceLine.rep) ~ restOfLine(start = significantVerticalSpaces.string <* !eol)).flatMap { case (index, (indentation, start)) =>
      ((restOfLine(start = indentation)) | (significantSpaceLine.rep.string ~ peek(indentation))).rep(0)
        .map { lines =>
          val sast: Seq[Sast] = scitzen.parser3.Parse.documentUnwrap((start :: lines).toList.mkString, Prov(index, indent = indentation.length))
          scitzen.sast.Parsed(indentation, sast)
        }
    }).map {
      case (parsed, prov) =>
        scitzen.sast.Block(Attributes(Nil), parsed, prov.copy(indent = parsed.delimiter.length))
    })

}

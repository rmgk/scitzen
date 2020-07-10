package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import Sast.{Fenced, Parsed, SBlock}
import scitzen.parser.CommonParsers._

object DelimitedBlockParsers {
  // use ` for verbatim text, : for parsed text
  def anyStart[_: P]: P[String] = P(CharIn(":`").rep(2).!)

  def makeDelimited[_: P](start: => P[String]): P[SBlock] =
    (start ~ MacroParsers.macroCommand.? ~ AttributesParser.braces.? ~ spaceLine ~/ Pass).flatMap {
      case (delimiter, command, attr) =>
        def closing = eol ~ delimiter ~ spaceLine
        (withProv(untilE(closing, min = 0)) ~ closing)
          .map {
            case (text, prov) =>
              val strippedText = stripIfPossible(text, delimiter.length)
              val rawAttr      = command.map(Attribute("", _)) ++: attr.getOrElse(Nil)
              val blockContent =
                delimiter(0) match {
                  case '`' => Fenced(strippedText)
                  case ':' =>
                    val sast: Seq[Sast] = Parse.documentUnwrap(strippedText, prov)
                    Parsed(delimiter, sast)
                }
              SBlock(Attributes(rawAttr, prov.copy(indent = delimiter.length)), blockContent)
          }
    }

  def anyDelimited[_: P]: P[SBlock] = P(makeDelimited(anyStart))


  val spaceNewline = " *\\n?$".r

  def stripIfPossible(str: String, i: Int): String = {
    val prefix = " ".repeat(i)
    str.linesWithSeparators.map { l =>
      if (l.startsWith(prefix)) l.substring(i)
      else if (spaceNewline.matches(l)) l
      else return str
    }.mkString
  }


  def whitespaceLiteral[_: P]: P[SBlock] =
    P(withProv((significantVerticalSpaces.! ~ !newline).flatMap { indentation =>
      (indentation.? ~ untilI(eol).!)
        .rep(min = 1, sep = significantSpaceLine.rep ~ &(indentation))
        .map { lines =>
          val sast: Seq[Sast] = Parse.documentUnwrap(lines.mkString, Prov())
          Parsed(indentation, sast)
        }
    }).map {
      case (parsed, prov) =>
        SBlock(Attributes(Nil, prov.copy(indent = parsed.delimiter.length)), parsed)
    })

}

package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.sast.{Attribute, Attributes, Block, Fenced, Prov, Sast}

object DelimitedBlockParsers {
  // use ` for verbatim text, : for parsed text
  def anyStart[_p: P]: P[String] = P(CharIn(":`").rep(2).!)

  def makeDelimited[_p: P](start: => P[String]): P[Block] =
    (start ~ MacroParsers.macroCommand.? ~ AttributesParser.braces.? ~ spaceLine ~/ Pass).flatMap {
      case (delimiter, command, attr) =>
        (withProv(untilI(eol ~ delimiter ~ spaceLine)))
          .map {
            case (text, prov) =>
              val (isStripped, strippedText) = stripIfPossible(text, delimiter.length)
              val rawAttr                    = command.map(Attribute("", _)) ++: attr.getOrElse(Nil)
              val blockContent =
                delimiter(0) match {
                  case '`' => Fenced(strippedText)
                  case ':' =>
                    val sast: Seq[Sast] = Parse.documentUnwrap(strippedText, prov)
                    scitzen.sast.Parsed(delimiter, sast)
                }
              scitzen.sast.Block(
                Attributes(rawAttr),
                blockContent,
                if (!isStripped) prov else prov.copy(indent = delimiter.length)
              )
          }
    }

  def anyDelimited[_p: P]: P[Block] = P(makeDelimited(anyStart))

  val spaceNewline = " *\\n?$".r

  def stripIfPossible(str: String, i: Int): (Boolean, String) = {
    val prefix = " ".repeat(i)
    true -> str.linesWithSeparators.map { l =>
      if (l.startsWith(prefix)) l.substring(i)
      else if (spaceNewline.matches(l)) l
      else return (false, str)
    }.mkString
  }

  def whitespaceLiteral[_p: P]: P[Block] =
    P(withProv((Index ~ significantSpaceLine.rep ~ significantVerticalSpaces.! ~ !eol ~ untilI(eol).!).flatMap { case (index, indentation, start) =>
      ((indentation ~ untilI(eol).!) | (significantSpaceLine.rep.! ~ &(indentation))).rep(0)
        .map { lines =>
          val sast: Seq[Sast] = Parse.documentUnwrap( (start +: lines).mkString, Prov(index, indent = indentation.length))
          scitzen.sast.Parsed(indentation, sast)
        }
    }).map {
      case (parsed, prov) =>
        scitzen.sast.Block(Attributes(Nil), parsed, prov.copy(indent = parsed.delimiter.length))
    })

}

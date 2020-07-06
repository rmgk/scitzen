package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._

object DelimitedBlockParsers {
  // we currently use ` for code, . for literal text,
  // : for nested content
  def anyStart[_: P]: P[String] = P(CharIn(":.=`").rep(2).!)

  def makeDelimited[_: P](start: => P[String]): P[NormalBlock] =
    (start ~ MacroParsers.macroCommand.? ~ AttributesParser.braces.? ~ spaceLine ~/ Pass)
      .flatMap {
        case (delimiter, command, attr) =>
          def closing = eol ~ delimiter ~ spaceLine

          (withProv(untilE(closing, min = 0)) ~ closing)
            .map {
              case (content, prov) =>
                val strippedContent = stripIfPossible(content, delimiter.length)
                NormalBlock(
                  delimiter.replace("=", ":"),
                  BlockCommand(command.getOrElse("")),
                  strippedContent,
                  Attributes(attr.getOrElse(Nil), prov.copy(indent = delimiter.length))
                )
            }
      }

  val spaceNewline = " *\\n?$".r

  def stripIfPossible(str: String, i: Int): String = {
    val prefix = " ".repeat(i)
    str.linesWithSeparators.map{ l =>
      if (l.startsWith(prefix)) l.substring(i)
      else if (spaceNewline.matches(l)) l
      else return str
    }.mkString
  }

  def full[_: P]: P[NormalBlock] = P(makeDelimited(anyStart))

  def whitespaceLiteral[_: P]: P[NormalBlock] =
    P(withProv((significantVerticalSpaces.! ~ !newline).flatMap { indentation =>
      (indentation.? ~ untilI(eol).!)
        .rep(min = 1, sep = significantSpaceLine.rep ~ &(indentation))
        .map(
          lines =>
            NormalBlock(
              indentation,
              BlockCommand(""),
              lines.mkString,
              Attributes.synthetic()
          )
        )
    }).map {
      case (nb, prov) =>
        nb.copy(attributes = nb.attributes.copy(prov = nb.attributes.prov.copy(indent = nb.delimiter.length)))
    })

}

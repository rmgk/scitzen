package scitzen.scipparse

import scitzen.sast.{Attribute, Attributes, Block, Fenced, Parsed, Prov, Sast}
import de.rmgk.scip.*
import CommonParsers.*
import CompatParsers.*

object DelimitedBlockParsers {
  // use ` for verbatim text, : for parsed text
  def anyStart: Scip[String] = (CharIn(":`").attempt.rep.require(_ >= 2).drop.str)

  def makeDelimited(start: Scip[String]): Scip[Block] = Scip {
    val delimiter    = start.run
    val command      = DirectiveParsers.macroCommand.opt.run
    val attr         = (AttributesParser.braces.opt <~ spaceLine).run
    val (text, prov) = withProv(untilI(eol ~ exact(delimiter) ~ spaceLine)).run
    val stripRes     = stripIfPossible(text, delimiter.length)
    val strippedText = stripRes.getOrElse(text)
    val rawAttr      = command.map(Attribute("", _)) ++: attr.getOrElse(Nil)
    val blockContent =
      delimiter(0) match {
        case '`' => Fenced(strippedText)
        case ':' =>
          val sast: Seq[Sast] = Parse.parserDocument.run(using Scx(strippedText))
          scitzen.sast.Parsed(delimiter, sast)
      }
    scitzen.sast.Block(
      Attributes(rawAttr),
      blockContent,
      if (stripRes.isEmpty) prov else prov.copy(indent = delimiter.length)
    )
  }

  def anyDelimited: Scip[Block] = (makeDelimited(anyStart))

  val spaceNewline = "^ *\\n?$".r

  def stripIfPossible(str: String, i: Int): Option[String] = {
    val prefix = " ".repeat(i)
    val lines  = str.linesWithSeparators
    val res    = new StringBuilder()
    while (lines.hasNext) {
      val l = lines.next()
      res.append {
        if (l.startsWith(prefix)) l.substring(i)
        else if (l.startsWith("\t")) l.substring(1)
        else if (spaceNewline.matches(l)) l
        else return None
      }
    }
    Some(res.result())
  }

  def whitespaceLiteral: Scip[Block] = Scip {
    val (parsed, prov) = withProv(Scip {
      val index = scx.index
      significantSpaceLine.attempt.rep.run
      val indentation = (significantVerticalSpaces.str <~ !eol).run
      val start       = untilI(eol).drop.str.run
      val indentP     = exact(indentation)
      val lines = choice(
        indentP ~> untilI(eol).drop.str,
        (significantSpaceLine.attempt.rep.drop.str <~ indentP.lookahead)
      ).list(Scip {}).run

      val sast: Seq[Sast] =
        Parse.parserDocument.run(using Scx((start +: lines).mkString)) // Prov(index, indent = indentation.length))
      scitzen.sast.Parsed(indentation, sast)
    }).run
    scitzen.sast.Block(Attributes(Nil), parsed, prov.copy(indent = parsed.delimiter.length))
  }

}

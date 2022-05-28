package scitzen.scipparse

import scitzen.sast.{Attribute, Attributes, Block, Fenced, Parsed, Prov, Sast}
import de.rmgk.scip.*
import CommonParsers.*
import CompatParsers.*

object DelimitedBlockParsers {
  // use ` for verbatim text, : for parsed text
  def anyStart: Scip[String] = (":`".any.rep.min(2).orFail.str)

  def makeDelimited(start: Scip[String]): Scip[Block] = Scip {
    val delimiter    = start.run
    val command      = DirectiveParsers.macroCommand.opt.trace("delimited marco").run
    val attr         = (AttributesParser.braces.opt <~ spaceLine).trace("delim braces").run
    val (text, prov) = withProv(untilI((eolB and exact(delimiter) and spaceLineB).trace(s"delimited closing ${delimiter}").orFail)).trace("delim block").run
    val stripRes     = stripIfPossible(text, delimiter.length)
    val strippedText = stripRes.getOrElse(text)
    val rawAttr      = command.map(Attribute("", _)) ++: attr.getOrElse(Nil)
    val blockContent =
      delimiter(0) match {
        case '`' => Fenced(strippedText)
        case ':' =>
          val sast: Seq[Sast] = Parse.parserDocument.trace("subparser delim").run(using Scx(strippedText).copy(tracing = scx.tracing, depth = scx.depth))
          scitzen.sast.Parsed(delimiter, sast)
      }
    scitzen.sast.Block(
      Attributes(rawAttr),
      blockContent,
      if (stripRes.isEmpty) prov else prov.copy(indent = delimiter.length)
    )
  }.trace("make delimited")

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
      val start       = (until(eolB).min(0) and eolB).orFail.str.run
      val indentP     = exact(indentation).orFail
      val lines = choice(
        indentP ~> untilI(eol).str,
        (significantSpaceLineB.rep.min(0).orFail.str <~ indentP.lookahead)
      ).list(Scip {true}).run

      val sast: Seq[Sast] =
        Parse.parserDocument.trace("subparser").run(using Scx((start +: lines).mkString).copy(tracing = scx.tracing, depth = scx.depth)) // Prov(index, indent = indentation.length))
      scitzen.sast.Parsed(indentation, sast)
    }).run
    scitzen.sast.Block(Attributes(Nil), parsed, prov.copy(indent = parsed.delimiter.length))
  }

}

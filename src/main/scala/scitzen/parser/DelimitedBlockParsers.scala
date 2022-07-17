package scitzen.parser

import de.rmgk.scip.*
import scitzen.sast.{Attribute, Attributes, Block, Fenced, Parsed, Prov, Sast}
import scitzen.parser.CommonParsers.*

object DelimitedBlockParsers {
  // use ` for verbatim text, : for parsed text
  def anyStart: Scip[String] = (":`".any.rep.min(2).str)

  def makeDelimited(start: Scip[String]): Scip[Block] = Scip {
    val delimiter    = start.run
    val command      = DirectiveParsers.macroCommand.opt.trace("delimited marco").run
    val attr         = (AttributesParser.braces.opt <~ spaceLineF).trace("delim braces").run
    val (text, prov) = withProv(untilIS(eolB and seq(delimiter) and spaceLineB)).trace("delim block").run
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
      significantSpaceLineB.rep.run
      val indentation = (significantVerticalSpacesB.str <~ eolB.lookahead.map(!_).orFail).run
      val start       = (untilI(eolB)).dropstr.run
      val indentP     = seq(indentation).orFail
      val lines = (
        (indentP ~> untilI(eolB).dropstr) |
        (significantSpaceLineB.rep.min(0).str <~ indentP.lookahead)
      ).list(Scip {true}).run

      val sast: Seq[Sast] =
        Parse.parserDocument.trace("subparser").run(using Scx((start +: lines).mkString).copy(tracing = scx.tracing, depth = scx.depth)) // Prov(index, indent = indentation.length))
      scitzen.sast.Parsed(indentation, sast)
    }).run
    scitzen.sast.Block(Attributes(Nil), parsed, prov.copy(indent = parsed.delimiter.length))
  }

}

package scitzen.parser

import de.rmgk.scip.*
import scitzen.parser.CommonParsers.*
import scitzen.sast.{Attribute, Attributes, BCommand, Block, Fenced, FusedDelimited, Prov, Sast}

object DelimitedBlockParsers {

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
}

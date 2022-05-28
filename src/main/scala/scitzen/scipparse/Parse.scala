package scitzen.scipparse

import scitzen.sast.{Inline, Prov, Sast}

import de.rmgk.scip.*

object Parse {

  def parseResult[T](content: Array[Byte], parser: Scip[T], prov: Prov = Prov()): T = {
    try parser.run0(Scx(content, 0, 0, -1, "", false))
    catch case f: ScipEx =>  throw IllegalStateException(f.getMessage)
  }

  val parserDocument: Scip[Seq[Sast]] = BlockParsers.alternatives.list(Scip { true }) <~ CompatParsers.End

  def documentUnwrap(blockContent: Array[Byte], prov: Prov): Seq[Sast] = {
    parseResult(blockContent, parserDocument)
  }

  val allInlines = InlineParsers(Scip { false }, CompatParsers.End)

  def inlineUnwrap(paragraphString: Array[Byte], prov: Prov): Seq[Inline] = {
    parseResult(paragraphString, allInlines.full)._1
  }
}

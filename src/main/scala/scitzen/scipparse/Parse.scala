package scitzen.scipparse

import scitzen.sast.{Inline, Prov, Sast}

import de.rmgk.scip.*

object Parse {

  def parseResult[T](content: Array[Byte], parser: Scip[T], prov: Prov = Prov()): T = {
    try
      parser.run0(Scx(
        input = content,
        index = 0,
        maxpos = content.length,
        depth = 0,
        lastFail = -1,
        reason = "",
        tracing = false
      ))
    catch case f: ScipEx => throw IllegalStateException(f.getMessage)
  }

  val parserDocument: Scip[Seq[Sast]] = BlockParsers.alternatives.list(Scip { true }) <~ (scipend.orFail)

  def documentUnwrap(blockContent: Array[Byte], prov: Prov): Seq[Sast] = {
    parseResult(blockContent, parserDocument)
  }

  val allInlines = InlineParsers(Scip { false }, scipend)

  def inlineUnwrap(paragraphString: Array[Byte], prov: Prov): Seq[Inline] = {
    parseResult(paragraphString, allInlines.full)._1
  }
}

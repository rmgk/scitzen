package scitzen.parser

import scitzen.sast.{Inline, Prov, Sast}

import de.rmgk.scip.*

object Parse {

  def parseResult[T](content: Array[Byte], parser: Scip[T], prov: Prov = Prov()): T = {
    val scx = Scx(
      input = content,
      index = 0,
      maxpos = content.length,
      depth = 0,
      lastFail = -1,
      tracing = false
    )
    try
      parser.runInContext(scx)
    catch
      case f: ScipEx => throw IllegalStateException(f.getMessage)
      case e: Exception =>
        throw IllegalStateException(s"parse exception: ${scx.ScipExInstance.getMessage}}", e)
  }

  val parserDocument: Scip[Seq[Sast]] = BlockParsers.alternatives.list(Scip { true }) <~ end.orFail

  def documentUnwrap(blockContent: Array[Byte], prov: Prov): Seq[Sast] = {
    parseResult(blockContent, parserDocument)
  }

  val allInlines = InlineParsers.full(end)

  def inlineUnwrap(paragraphString: Array[Byte], prov: Prov): Seq[Inline] = {
    parseResult(paragraphString, allInlines)
  }
}

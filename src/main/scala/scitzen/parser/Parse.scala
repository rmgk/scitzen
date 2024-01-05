package scitzen.parser

import de.rmgk.scip.*
import scitzen.fusion.Fusion
import scitzen.project.Document
import scitzen.sast.{Inline, Prov, Sast}

object Parse {

  def parseResult[T](content: Array[Byte], parser: Scip[T], prov: Prov = Prov()): T = {
    def newscx() = Scx(
      input = content,
      index = 0,
      maxpos = content.length,
      depth = 0,
      lastFail = -1,
      tracing = false
    )
    val scx = newscx()
    try
      parser.runInContext(scx)
    catch
      case f: ScipEx =>
        try parser.runInContext(newscx().copy(tracing = true))
        catch
          case f: ScipEx => ()
        throw IllegalStateException(f.getMessage)
      case e: Exception =>
        throw IllegalStateException(s"parse exception: ${scx.ScipExInstance.getMessage}}", e)
  }

  val parserDocument: Scip[List[Sast]] = BlockParsers.alternatives.list(Scip { true }) <~ end.orFail

  def documentUnwrap(doc: Document): List[Sast] = {
    //parseResult(doc.content, parserDocument)
    Fusion.documentUnwrap(doc)
  }

  val allInlines = InlineParsers.full(end)

  def inlineUnwrap(paragraphString: Array[Byte]): List[Inline] = {
    parseResult(paragraphString, allInlines)
  }

  def bibfileUnwrap(bibfile: Array[Byte]): List[Biblet] = {
    parseResult(bibfile, BibPreparser.entry.list(Scip(true)))
  }
}

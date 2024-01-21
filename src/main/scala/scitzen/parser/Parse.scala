package scitzen.parser

import de.rmgk.scip.*
import scitzen.cli.Logging
import scitzen.sast.Fusion.Atoms
import scitzen.project.Document
import scitzen.sast.{Fusion, Inline, Prov, Sast}

object Parse {

  def parseResult[T](content: Array[Byte], parser: Scip[T], document: Option[Document] = None): T = {
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
        Logging.cli.warn(s"error parsing ${document.map { _.path }}")
        try parser.runInContext(newscx().copy(tracing = true))
        catch
          case f: ScipEx => ()
        throw IllegalStateException(f.getMessage)
      case e: Exception =>
        throw IllegalStateException(s"parse exception: ${scx.ScipExInstance.getMessage}}", e)
  }

  val parserDocument: Scip[List[Sast]] = Fusion.parser

  def documentUnwrap(doc: Document): List[Sast] = {
    parseResult(
      doc.content,
      Fusion.parser,
      Some(doc)
    )
  }

  def atoms(doc: Document): Atoms = {
    parseResult(
      doc.content,
      Fusion.atoms,
      Some(doc)
    )
  }

  def bibfileUnwrap(bibfile: Array[Byte]): List[Biblet] = {
    parseResult(bibfile, BibPreparser.entry.list(Scip(true)))
  }
}

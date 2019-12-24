package scitzen.generic

import better.files.File
import scitzen.parser.MacroCommand.Include

object Months {
  val en = Array("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")
}

case class FullDoc(parsed: ParsedDocument, analyzed: AnalyzedDoc)

class DocumentManager(val documents: List[ParsedDocument]) {

  lazy val byPath: Map[File, FullDoc] =
    fulldocs.map(fd => fd.parsed.file -> fd).toMap

  lazy val analyzed: List[AnalyzedDoc] = documents.map(AnalyzedDoc.apply)

  lazy val fulldocs: List[FullDoc] =
    documents.zip(analyzed)
             .map { case (a, b) => FullDoc(a, b) }

  lazy val attributes: Map[String, String] = analyzed.flatMap(_.named).toMap

}

object DocumentManager {
  @scala.annotation.tailrec
  def resolveIncludes(documentManager: DocumentManager): DocumentManager = {
    val includes = (for {
      fulldoc <- documentManager.fulldocs
      doc = fulldoc.parsed
      analyzed = fulldoc.analyzed
      mcro <- analyzed.analyzeResult.macros
      if mcro.command == Include && !(mcro.attributes.positional.size > 1 && mcro.attributes.positional.head == "code")
      file = doc.file.parent / mcro.attributes.target
      exists = if (file.isRegularFile) true else {
        scribe.warn(s"Included file »${File.currentWorkingDirectory.relativize(file)}« does not exist" + analyzed.analyzes.macroReporter(mcro))
        false
      }
      if exists & !documentManager.byPath.contains(file)
    } yield file).toSet
    if (includes.isEmpty) documentManager
    else {
      val newPF = includes.iterator.map(ParsedDocument.apply) ++: documentManager.documents
      resolveIncludes(new DocumentManager(newPF))
    }
  }
}

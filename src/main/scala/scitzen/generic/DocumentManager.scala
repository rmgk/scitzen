package scitzen.generic

import better.files.File
import scitzen.parser.MacroCommand.Include

object Months {
  val en = Array("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")
}

class DocumentManager(_documents: List[ParsedDocument]) {


  def relTargetPath(root: File, post: ParsedDocument) = {
    "posts/" + post.file.name.toString.replace(".scim", ".html")
  }


  def documents: List[ParsedDocument] = _documents
  val byPath: Map[File, ParsedDocument] = _documents.map(pd => pd.file -> pd).toMap

  lazy val attributes: Map[String, String] = documents.flatMap(_.sdoc.named).toMap

}

object DocumentManager {
  @scala.annotation.tailrec
  def resolveIncludes(documentManager: DocumentManager): DocumentManager = {
    val includes = (for {
      doc <- documentManager.documents
      mcro <- doc.sdoc.analyzeResult.macros
      if mcro.command == Include
      file = doc.file.parent / mcro.attributes.target
      exists = if (file.isRegularFile) true else {
        scribe.warn(s"Included file »${File.currentWorkingDirectory.relativize(file)}« does not exist" + doc.reporter(mcro))
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

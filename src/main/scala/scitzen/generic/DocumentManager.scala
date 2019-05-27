package scitzen.generic

import better.files.File

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


  def find(root: File, path: String): Option[ParsedDocument] = {
    byPath.get(root / path).filter(d => root.isParentOf(d.file))
  }

}

object DocumentManager {
  @scala.annotation.tailrec
  def resolveIncludes(documentManager: DocumentManager): DocumentManager = {
    val includes = (for {
      docs <- documentManager.documents
      macrs <- docs.sdoc.analyzeResult.macros
      if macrs.command == "include"
      file = docs.file.parent / macrs.attributes.target
      if !documentManager.byPath.contains(file)
    } yield file).toSet
    if (includes.isEmpty) documentManager
    else {
      scribe.info(s"found includes: $includes")
      val newPF = includes.iterator.map(ParsedDocument.apply) ++: documentManager.documents
      resolveIncludes(new DocumentManager(newPF))
    }
  }
}

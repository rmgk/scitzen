package scitzen.generic

import better.files.File

case class DocumentDirectory(documents: List[Document]) {
  lazy val byPath: Map[File, Document] =
    documents.map(fd => fd.file -> fd).toMap
}
object DocumentDirectory {
  def apply(root: File): DocumentDirectory = {
    scribe.debug(s"discovering sources in $root")
    val sources: List[File] = Project.discoverSources(root)
    scribe.debug(s"parsing ${sources.length} documents")
    val documents = sources.map(Document.apply)
    DocumentDirectory(documents)
  }
}

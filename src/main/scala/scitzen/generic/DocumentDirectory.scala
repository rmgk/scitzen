package scitzen.generic

import better.files.File

case class DocumentDirectory(documents: List[Document]) {
  lazy val byPath: Map[File, Document] =
    documents.map(fd => fd.file -> fd).toMap
}

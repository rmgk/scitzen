package scitzen.generic

import java.nio.file.Path

case class DocumentDirectory(documents: List[Document]):
  lazy val byPath: Map[Path, Document] =
    documents.map(fd => fd.file -> fd).toMap

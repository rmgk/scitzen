package scitzen.generic

import better.files.File
import scitzen.cli.ParsedDocument

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


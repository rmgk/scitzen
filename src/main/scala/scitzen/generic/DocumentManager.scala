package scitzen.generic

import better.files.File


case class FullDoc(parsed: ParsedDocument, analyzed: AnalyzedDoc) {
  def sast: List[Sast] = analyzed.sast

}

class DocumentManager(root: File) {

  lazy val sources: List[File] = {
    scribe.debug(s"discovering sources in $root")
    Project.discoverSources(root)
  }

  lazy val documents: List[ParsedDocument] = {
    scribe.debug(s"parsing ${sources.length} documents")
    sources.map(ParsedDocument.apply)
  }

  lazy val analyzed: List[AnalyzedDoc] = documents.map(AnalyzedDoc.apply)

  lazy val fulldocs: List[FullDoc] =
    documents.zip(analyzed)
             .map { case (a, b) => FullDoc(a, b) }

  lazy val byPath: Map[File, FullDoc] =
    fulldocs.map(fd => fd.parsed.file -> fd).toMap

  lazy val attributes: Map[String, String] = analyzed.flatMap(_.named).toMap

}

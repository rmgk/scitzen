package scitzen.cli

import java.nio.file.Path

import better.files._
import fastparse.Parsed.{Failure, Success}
import scitzen.converter.Post
import scitzen.parser.{Document, DocumentParsers}

class PostFolder(basedir: Path) {
  def makePost(path: Path): Post = {
    val content = File(path).contentAsString
    val document = fastparse.parse(content, DocumentParsers.document(_)) match {
      case f: Failure =>
        scribe.error(f.trace().longMsg)
        f.get
      case s: Success[Document] => s.get.value
    }

    new Post(document, targetPath(path), path.toString, content)
  }


  def targetPath(path: Path): String = basedir.relativize(path).toString.replace(".adoc", ".html").replace(".scim", ".html")

}

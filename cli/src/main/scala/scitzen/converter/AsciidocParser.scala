package scitzen.converter

import java.nio.file.Path

import better.files._
import scitzen.parser.DocumentParsers

class AsciidocParser(basedir: Path) {
  def makePost(path: Path): Post = {
    val document = fastparse.parse(File(path).contentAsString, DocumentParsers.document(_)).get.value
    new Post(document, targetPath(path))
  }


  def targetPath(path: Path): String = basedir.relativize(path).toString.replace(".adoc", ".html")

}

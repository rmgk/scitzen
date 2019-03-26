package scitzen.cli

import java.nio.file.Path

import better.files._
import scitzen.converter.Post
import scitzen.parser.DocumentParsers

class PostFolder(basedir: Path) {
  def makePost(path: Path): Post = {
    val content = File(path).contentAsString
    val document = fastparse.parse(content, DocumentParsers.document(_)).get.value
    new Post(document, targetPath(path), path.toString, content)
  }


  def targetPath(path: Path): String = basedir.relativize(path).toString.replace(".adoc", ".html")

}

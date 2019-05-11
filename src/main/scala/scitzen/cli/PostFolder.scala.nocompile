package scitzen.cli

import java.nio.file.Path

import better.files._
import scitzen.converter.Post
import scitzen.parser.Parse

class PostFolder(basedir: Path) {
  def makePost(path: Path): Post = {
    val content = File(path).contentAsString
    val document = Parse.document(content).right.get
    new Post(document, targetPath(path), path.toString, content)
  }


  def targetPath(path: Path): String = basedir.relativize(path).toString.replace(".adoc", ".html").replace(".scim", ".html")

}

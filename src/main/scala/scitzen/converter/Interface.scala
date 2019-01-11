package scitzen.converter

import java.nio.file.Path
import java.time.LocalDateTime

trait AdocParser {
  def makePost(path: Path): Post
}

trait Post {
  val path: Path
  def people(): List[String]
  def folder(): Option[String]
  def categories(): List[String]
  def targetPath(): String = path.toString.replace(".adoc", ".html")
  def title: String
  def date: LocalDateTime
  def content: String
  def modified: Option[LocalDateTime]
}
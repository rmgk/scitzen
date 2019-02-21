package scitzen.converter

import java.nio.file.Path

import better.files._
import scitzen.parser.{DateParsingHelper, Document, DocumentParsers, ScitzenDateTime}

class AsciidocParser(basedir: Path) {
  def makePost(path: Path): Post = {
    val document = fastparse.parse(File(path).contentAsString, DocumentParsers.document(_)).get.value
    new Post(basedir.relativize(path), document)
  }
}

class Post(val path: Path, val document: Document) {
  val attributes: Map[String, String] = document.header.get.attributes.map(a => a.id -> a.value).toMap
  def commaSeparatedAttribute(key: String): List[String] =
    attributes.getOrElse(key, "").toString
    .split(',')
    .map[String, List[String]](_.trim)(collection.breakOut)
    .filter(_.nonEmpty)

  def people(): List[String] = commaSeparatedAttribute("people")

  def folder(): Option[String] = attributes.get("folder")

  def categories(): List[String] = commaSeparatedAttribute("categories")

  def title: String = document.header.fold("(null)")(_.title)
  lazy val date: Option[ScitzenDateTime] = attributes.get("revdate").map(v => DateParsingHelper.parseDate(v.trim))
  def content: String = HtmlConverter.convert(document)
  lazy val modified: Option[ScitzenDateTime] = attributes.get("modified")
                                             .map(m => DateParsingHelper.parseDate(m.trim))

  def targetPath(): String = path.toString.replace(".adoc", ".html")

}


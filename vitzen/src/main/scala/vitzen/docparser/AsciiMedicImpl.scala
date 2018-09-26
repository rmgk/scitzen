package vitzen.docparser

import java.nio.file.Path
import java.time.LocalDateTime

import asciimedic.{Asciimedic, Document}
import better.files._
import vitzen.DateParsingHelper

class AsciiMedicImpl(basedir: Path) {
  def makePost(path: Path): Post = {
    val document = Asciimedic.document.parse(File(path).contentAsString).get.value
    new MedicPost(basedir.relativize(path), document)
  }
}

class MedicPost(val path: Path, val document: Document) extends Post {
  val attributes = document.header.get.attributes.map(a => a.id -> a.value).toMap
  def commaSeparatedAttribute(key: String): List[String] =
    attributes.getOrElse(key, "").toString
    .split(',')
    .map[String, List[String]](_.trim)(collection.breakOut)
    .filter(_.nonEmpty)

  def people(): List[String] = commaSeparatedAttribute("people")

  def folder(): Option[String] = attributes.get("folder")

  def categories(): List[String] = commaSeparatedAttribute("categories")

  def title: String = document.header.fold("(null)")(_.title)
  lazy val date: LocalDateTime = attributes.get("revdate")
                                 .fold(LocalDateTime.MIN)(v => DateParsingHelper.parseDate(v.trim))
  def content: String = HtmlConverter.convert(document)
  lazy val modified: Option[LocalDateTime] = attributes.get("modified")
                                             .map(m => DateParsingHelper.parseDate(m.trim))
}


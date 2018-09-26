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

  def folder(): Option[String] = Option(attributes.get("folder")).map(_.toString)

  def categories(): List[String] = commaSeparatedAttribute("categories")

  def title: String = Option(document.header.get.title).getOrElse("(null)")
  lazy val date: LocalDateTime = Option(attributes.get("revdate"))
                                 .fold(LocalDateTime.MIN)(v => DateParsingHelper.parseDate(v.toString))
  def content: String = document.blocks.toString()
  lazy val modified: Option[LocalDateTime] = Option(attributes.get("modified"))
                                             .map(m => DateParsingHelper.parseDate(m.toString))
}

package vitzen.docparser

import java.nio.file.Path
import java.time.LocalDateTime
import java.util

import org.asciidoctor.ast.Document
import org.asciidoctor.{Asciidoctor, OptionsBuilder, SafeMode}
import vitzen.DateParsingHelper

class AsciiData(basedir: Path) {
  val asciidoctor: Asciidoctor = Asciidoctor.Factory.create()

  val options: util.Map[String, AnyRef] =
    OptionsBuilder.options().headerFooter(false).safe(SafeMode.SERVER).asMap()


  def makePost(path: Path): Post = {
    val document = asciidoctor.loadFile(path.toFile, options)
    val opts = new util.HashMap[AnyRef, AnyRef]
    opts.put("partition", boolean2Boolean(true))

    new DocPost(basedir.relativize(path), document)
  }


}





class DocPost(val path: Path, val document: Document) extends Post {
  def commaSeparatedAttribute(key: String): List[String] =
    document.getAttributes.getOrDefault(key, "").toString
    .split(',')
    .map[String, List[String]](_.trim)(collection.breakOut)
    .filter(_.nonEmpty)

  def people(): List[String] = commaSeparatedAttribute("people")

  def folder(): Option[String] = Option(document.getAttributes.get("folder")).map(_.toString)

  def categories(): List[String] = commaSeparatedAttribute("categories")

  def summary(): String = Option(document.getBlocks.get(0)).fold("")(b => b.convert())

  def title: String = Option(document.getDoctitle).getOrElse("(null)")
  lazy val date: LocalDateTime = Option(document.getAttributes.get("revdate"))
                                 .fold(LocalDateTime.MIN)(v => DateParsingHelper.parseDate(v.toString))
  def content: String = document.convert()
  lazy val modified: Option[LocalDateTime] = Option(document.getAttributes.get("modified"))
                                             .map(m => DateParsingHelper.parseDate(m.toString))
}

package vitzen

import java.io.File
import java.nio.file.Path
import java.time.LocalDateTime
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.util

import org.asciidoctor.ast.Document
import org.asciidoctor.{AsciiDocDirectoryWalker, Asciidoctor, OptionsBuilder, SafeMode}

import scala.collection.JavaConverters._

object Helper {
  val timeFormatter: DateTimeFormatter = new DateTimeFormatterBuilder()
  .parseCaseInsensitive()
  .append(DateTimeFormatter.ISO_LOCAL_DATE)
  .optionalStart()
  .optionalStart().appendLiteral('T').optionalEnd()
  .optionalStart().appendLiteral(' ').optionalEnd()
  .append(DateTimeFormatter.ISO_LOCAL_TIME)
  .optionalEnd()
  .optionalStart().appendOffsetId().optionalEnd()
  .toFormatter()

  def parseDate(dateString: String): LocalDateTime = {
    if (dateString == null) return LocalDateTime.MIN
    val temporal = Helper.timeFormatter.parse(dateString)
    LocalDateTime.from(temporal)
  }

}

class AsciiData(asciidoctor: Asciidoctor, basedir: Path) {

  val options: util.Map[String, AnyRef] = OptionsBuilder.options().headerFooter(false).safe(SafeMode.SERVER).asMap()

  def getOne(pathString: String): Post = {
    val path = basedir.resolve(pathString)
    makePost(path)
  }


  def makePost(path: Path): Post = {
    val document = asciidoctor.loadFile(path.toFile, options)
    val opts = new util.HashMap[AnyRef, AnyRef]
    opts.put("partition", boolean2Boolean(true))

    new Post(basedir.relativize(path), document)
  }

  def allFiles(): List[File] = new AsciiDocDirectoryWalker(basedir.toString).scan().iterator().asScala.toList

  def allPosts(): List[Post] = allFiles().map { f: File => makePost(f.toPath) }
}


class Post(val path: Path, val document: Document) {
  def commaSeparatedAttribute(key: String): List[String] =
    document.getAttributes.getOrDefault(key, "").toString
    .split(',')
    .map[String, List[String]](_.trim)(collection.breakOut)
    .filter(_.nonEmpty)

  def people(): List[String] = commaSeparatedAttribute("people")

  def folder(): Option[String] = Option(document.getAttributes.get("folder")).map(_.toString)

  def categories(): List[String] = commaSeparatedAttribute("categories")

  def targetPath(): String = path.toString.replace(".adoc", ".html")
  def summary(): String = Option(document.getBlocks.get(0)).fold("")(b => b.convert())

  def title: String = Option(document.getDoctitle).getOrElse("(null)")
  lazy val date: LocalDateTime = Option(document.getAttributes.get("revdate"))
                                 .fold(LocalDateTime.MIN)(v => Helper.parseDate(v.toString))
  def content: String = document.convert()
  lazy val modified: Option[LocalDateTime] = Option(document.getAttributes.get("modified"))
                                             .map(m => Helper.parseDate(m.toString))
}

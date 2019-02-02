package scitzen.pages

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path
import java.time.LocalDateTime
import java.util.NoSuchElementException

import better.files.File
import com.monovore.decline.{Command, Opts}
import scitzen.converter.DateParsingHelper
import scitzen.parser.{DocumentParsers, Header}

object Tool {
  def sluggify(str: String): String =
    str
    .trim
    .replace("'", "")
    .replaceAll("""[^\p{L}\d]""", "-")
    .replaceAll("-+", "-")
    .replaceAll("^-|-$", "")
}

object Rename {

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8

  val command: Command[Unit] = Command(
    name = "rename",
    header = "Auto generate file names for posts based on their titles and dates."
  ) {
    val optSource = Opts.arguments[Path](metavar = "paths")

    optSource.map { sourcefiles =>
      sourcefiles.map(File(_))
      .filter(f => f.isRegularFile && f.name.endsWith(".adoc"))
      .foreach(renameFileFromHeader)
    }
  }

  def renameFileFromHeader(f: File): Unit = {
    val header: Header = fastparse.parse(f.contentAsString, DocumentParsers.header(_)).get.value
    val newName: String = nameFromHeader(header)

    if (newName != f.name) {
      println(s"rename ${f.name} to $newName")
      f.renameTo(newName)
    }
  }

  def nameFromHeader(header: Header): String = {
    val headerDateString =
      header.attributes.map(a => a.id -> a.value).toMap
      .getOrElse("revdate",
                 throw new NoSuchElementException(s"${header.title} has no revdate")).trim
    val date = parseDate(headerDateString)
    val title = Tool.sluggify(header.title) + ".adoc"
    date.format(DateParsingHelper.dateOnlyOutput) + "_" + title
  }
  private def parseDate(headerDateString: String): LocalDateTime = {
    LocalDateTime.from(DateParsingHelper.relaxedISODateTimeParser.parse(headerDateString))
  }
}

package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files.File
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import scitzen.parser.{Parse, DateParsingHelper, Document}


object Rename {

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8

  val command: Command[Unit] = Command(
    name = "rename",
    header = "Auto generate file names for posts based on their titles and dates."
  ) {
    val optSource = Opts.arguments[Path](metavar = "paths")

    optSource.map { sourcefiles =>
      sourcefiles.map(File(_))
      .filter(f => f.isRegularFile && f.name.endsWith(".scim"))
      .foreach(renameFileFromHeader)
    }
  }

  def renameFileFromHeader(f: File): Unit = {
    val header: Document = Parse.document(f.contentAsString).valueOr(throw _)
    val newName: String = nameFromHeader(header)

    if (newName != f.name) {
      println(s"rename ${f.name} to $newName")
      f.renameTo(newName)
    }
  }


  def nameFromHeader(header: Document): String = {
    val date = DateParsingHelper.parseDate(header.attributes.named("revdate").trim)
    val title = sluggify(header.title) + ".scim"
    date.date.full + "_" + title
  }

  def sluggify(str: String): String =
    str
    .trim
    .replace("'", "")
    .replaceAll("""[^\p{L}\d]""", "-")
    .replaceAll("-+", "-")
    .replaceAll("^-|-$", "")

}

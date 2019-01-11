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
  def sluggify(str: String): String = str
                                      .trim
                                      .replace("'", "")
                                      .replaceAll("""[^\p{L}\d]""", "-")
                                      .replaceAll("-+", "-")
                                      .replaceAll("^-|-$", "")
}

object Renaming {
  val command = Command(
    name = "rename",
    header = "Auto generate file names for posts based on their titles and dates.") {
    implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8
    val optSource = Opts.option[Path]("source", short = "s", metavar = "directory",
                                      help = "Directory containing Asciidoc source posts")


    optSource.map { sourceP =>
      val source = File(sourceP)
      source.children.filter(_.isRegularFile).filter(_.name.endsWith(".adoc")).foreach { f =>
        println(f.name)
        val header: Header = fastparse.parse(f.contentAsString, DocumentParsers.header(_)).get.value
        val date = LocalDateTime.from(DateParsingHelper.timeFormatter.parse(
          header.attributes.map(a => a.id -> a.value).toMap
          .getOrElse("revdate", throw new NoSuchElementException(s"${header.title} has no revdate")).trim))
        val title = Tool.sluggify(header.title) + ".adoc"
        val newName = date.format(DateParsingHelper.outputTime) + "_" + title

        if (newName != f.name) {
          println(f.name)
          println(newName)
          f.renameTo(newName)
        }

      }
    }
  }
}

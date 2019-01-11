package vitzen

import java.nio.file.Path
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.util.NoSuchElementException

import asciimedic.{Asciimedic, Header}
import better.files.File
import com.monovore.decline.{CommandApp, Opts}

object Tool {
  def sluggify(str: String): String = str
                                      .trim
                                      .replace("'", "")
                                      .replaceAll("""[^\p{L}\d]""", "-")
                                      .replaceAll("-+", "-")
                                      .replaceAll("^-|-$", "")
}

object Renaming extends CommandApp(
  name = "asciidocii",
  header = "Tools for parsing asciidoc and doing something with the result",
  main = {
    implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8
    val optSource = Opts.option[Path]("source", short = "s", metavar = "directory",
                                      help = "Directory containing Asciidoc source posts")


    optSource.map { sourceP =>
      val source = File(sourceP)
      source.children.filter(_.isRegularFile).filter(_.name.endsWith(".adoc")).foreach { f =>
        println(f.name)
        val header: Header = fastparse.parse(f.contentAsString, Asciimedic.header(_)).get.value
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
)

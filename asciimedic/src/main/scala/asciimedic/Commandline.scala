package asciimedic

import java.nio.file.Path
import java.time.LocalDateTime
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.util.NoSuchElementException

import better.files.File
import com.monovore.decline.{CommandApp, Opts}

object Tool {
  val timeFormatter: DateTimeFormatter =
    new DateTimeFormatterBuilder()
    .parseCaseInsensitive()
    .append(DateTimeFormatter.ISO_LOCAL_DATE)
    .optionalStart()
    .optionalStart().appendLiteral('T').optionalEnd()
    .optionalStart().appendLiteral(' ').optionalEnd()
    .append(DateTimeFormatter.ISO_LOCAL_TIME)
    .optionalEnd()
    .optionalStart().appendOffsetId().optionalEnd()
    .toFormatter()

  val outputTime: DateTimeFormatter =
    new DateTimeFormatterBuilder()
    .append(DateTimeFormatter.ISO_LOCAL_DATE)
//    .appendLiteral("_")
//    .appendValue(ChronoField.HOUR_OF_DAY, 2)
//    .appendLiteral("-")
//    .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
    .toFormatter()

  def sluggify(str: String): String = str
                                      .trim
                                      .replace("'", "")
                                      .replaceAll("""[^\p{L}\d]""", "-")
                                      .replaceAll("-+", "-")
                                      .replaceAll("^-|-$", "")
}

object Commandline extends CommandApp(
  name = "asciidocii",
  header = "Tools for parsing asciidoc and doing something with the result",
  main = {
    val optSource = Opts.option[Path]("source", short = "s", metavar = "directory",
                                      help = "Directory containing Asciidoc source posts")


    optSource.map { sourceP =>
      val source = File(sourceP)
      source.children.filter(_.isRegularFile).filter(_.name.endsWith(".adoc")).foreach { f =>
        println(f.name)
        val header: Header = Asciimedic.HeaderParser.header.parse(f.contentAsString).get.value
        val date = LocalDateTime.from(Tool.timeFormatter.parse(
          header.attributes.map(a => a.id -> a.value).toMap
          .getOrElse("revdate", throw new NoSuchElementException(s"${header.title} has no revdate")).trim))
        val title = Tool.sluggify(header.title) + ".adoc"
        val newName = date.format(Tool.outputTime) + "_" + title

        if (newName != f.name) {
          println(f.name)
          println(newName)
          f.renameTo(newName)
        }

      }
    }
  }
)

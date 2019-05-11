package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files.File
import cats.data.NonEmptyList
import com.monovore.decline.{Command, Opts}
import scitzen.converter.SastToScimConverter
import scitzen.parser.DateParsingHelper
import scitzen.semantics.{SastConverter, Sdoc}

case class DocumentDiscovery(sourcePaths: List[File]) {

  val fileEnding  = "scim"
  val globPattern = "*." + fileEnding


  lazy val sourceDirectories = sourcePaths.filter(_.isDirectory)
  lazy val sourceFiles       = sourcePaths.flatMap {
    case f if f.isRegularFile => List(f)
    case f if f.isDirectory   => f.glob(globPattern).toList
  }
                               // extension also checks if the file is a regular file and exists
                               .filter(f => f.extension(includeDot = false,
                                                        toLowerCase = true).contains(fileEnding))
}
object DocumentDiscovery {
  def apply(nonEmptyList: NonEmptyList[Path]): DocumentDiscovery =
    DocumentDiscovery(nonEmptyList.map(File(_)).toList)
}


object Format {

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8


  val pathsOpt: Opts[NonEmptyList[Path]] = Opts.arguments[Path](metavar = "paths")

  val command: Command[Unit] = Command(name = "format",
                                       header = "Convert Scim to Scim") {

    pathsOpt.map { paths =>
      val dd = DocumentDiscovery(paths)
      dd.sourceFiles.foreach { file =>
        val content = file.contentAsString
        val sast = SastConverter().documentString(content)
        val sdoc = Sdoc(sast)
        val result = SastToScimConverter().toScim(sast)
        val resultStr = result.mkString("", "\n", "\n")
        if (resultStr != content) file.write(resultStr)
        renameFileFromHeader(file, sdoc)
      }
    }
  }


  def renameFileFromHeader(f: File, sdoc: Sdoc): Unit = {
    val newName: String = nameFromHeader(sdoc)

    if (newName != f.name) {
      println(s"rename ${f.name} to $newName")
      f.renameTo(newName)
    }
  }


  def nameFromHeader(header: Sdoc): String = {
    val date = DateParsingHelper.parseDate(header.named("revdate").trim)
    val title = sluggify(header.title.get) + ".scim"
    date.date.full + " " + title
  }

  def sluggify(str: String): String =
    str
    .replaceAll("""[<>":;%/\?\[\]\\\*\|]""", "-")
    .replaceAll("\\s+", " ")
    .replaceAll("-+", "-")
    .trim
    .replaceAll("^-|-$", "")

}

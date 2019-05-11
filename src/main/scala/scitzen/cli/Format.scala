package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files.File
import cats.data.NonEmptyList
import com.monovore.decline.{Command, Opts}
import scitzen.converter.SastToScimConverter
import scitzen.parser.DateParsingHelper
import scitzen.semantics.{Sast, SastConverter, Sdoc}

case class DocumentDiscovery(sourcePaths: List[File]) {

  val fileEnding  = "scim"
  val globPattern = "**." + fileEnding

  lazy val sourceDirectories: List[File] = sourcePaths.filter(_.isDirectory)
  lazy val sourceFiles      : List[File] = sourcePaths.flatMap {
    case f if f.isRegularFile => List(f)
    case f if f.isDirectory   =>
      f.glob(globPattern).toList
      // extension also checks if the file is a regular file and exists
      .filter(f => f.extension(includeDot = false, toLowerCase = true).contains(fileEnding))
  }
}
object DocumentDiscovery {
  def apply(nonEmptyList: NonEmptyList[Path]): DocumentDiscovery =
    DocumentDiscovery(nonEmptyList.map(File(_)).toList)
}

final case class ParsedDocument(file: File, content: String, sast: Seq[Sast], sdoc: Sdoc)
object ParsedDocument {
  def apply(file: File): ParsedDocument = {
    val content = file.contentAsString
    val sast = SastConverter().documentString(content)
    val sdoc = Sdoc(sast)
    ParsedDocument(file, content, sast, sdoc)
  }
}


object Format {

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8


  val pathsOpt: Opts[NonEmptyList[Path]] = Opts.arguments[Path](metavar = "paths")

  val command: Command[Unit] = Command(name = "format",
                                       header = "Convert Scim to Scim") {

    pathsOpt.map { paths =>
      val dd = DocumentDiscovery(paths)
      dd.sourceFiles.foreach { file =>
        val ParsedDocument(_, content, sast, sdoc) = ParsedDocument(file)
        checkReferences(file, sdoc)
        formatContent(file, content, sast)
        renameFileFromHeader(file, sdoc)
      }
    }
  }


  def checkReferences(file: File, sdoc: Sdoc): Unit = {
    sdoc.analyzeResult.macros.foreach { mcro =>
      mcro.command match {
        case "image" =>
          val path = file.parent./(mcro.attributes.target.trim)
          if (path.isRegularFile && file.parent.isParentOf(path)) ()
          else scribe.warn(s"${file} references nonexisting $path")
        case other   => ()
      }
    }
  }

  def formatContent(file: File, originalContent: String, sast: Seq[Sast]): Unit = {
    val result = SastToScimConverter().toScim(sast)
    val resultStr = result.mkString("", "\n", "\n")
    if (resultStr != originalContent) {
      scribe.info(s"formatting ${file.name}")
      file.write(resultStr)
    }
  }


  def renameFileFromHeader(f: File, sdoc: Sdoc): Unit = {
    val newName: String = nameFromHeader(sdoc)

    if (newName != f.name) {
      scribe.info(s"rename ${f.name} to $newName")
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

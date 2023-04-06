package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import scitzen.generic.{Article, DocumentDirectory}
import scitzen.outputs.SastToScimConverter
import scitzen.sast.Sast
import scitzen.compat.Logging.scribe

import java.nio.file.{Files, Path}

object Format:

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8

  def formatContents(documentDirectory: DocumentDirectory): Unit =
    documentDirectory.documents.foreach { pd =>
      formatContent(pd.file, pd.content, pd.sast)
    }

  def formatRename(documentDirectory: DocumentDirectory): Unit =
    documentDirectory.documents.foreach { parsed =>
      Article.articles(parsed) match
        case List(article) if (article.date.isDefined) =>
          renameFileFromHeader(parsed.file, article)
        case _ =>
          scribe.debug(
            s"could not format ${parsed.file}, did not contain a single article with a date"
          )
    }

  def formatContent(file: Path, originalContent: Array[Byte], sast: Seq[Sast]): Unit =
    val result      = SastToScimConverter.toScimS(sast)
    val resultBytes = result.iterator.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8)
    if !java.util.Arrays.equals(resultBytes, originalContent) then
      scribe.info(s"formatting ${file.getFileName}")
      Files.write(file, resultBytes)

  def renameFileFromHeader(f: Path, sdoc: Article): Unit =
    val newName: String = canonicalName(sdoc) + ".scim"

    if newName != f.getFileName.toString then
      scribe.info(s"rename ${f.getFileName} to $newName")
      Files.move(f, f.resolveSibling(newName))

  def canonicalName(header: Article): String =
    val title = sluggify(header.filename.getOrElse(header.title))
    header.date.map(_.date.full).fold(title)(d => d + " " + title)

  def sluggify(str: String): String =
    str
      .replaceAll("""[<>":;%/\?\[\]\\\*\|]""", "-")
      .replaceAll("\\s+", " ")
      .replaceAll("-+", "-")
      .trim
      .replaceAll("^-|-$", "")
      .trim

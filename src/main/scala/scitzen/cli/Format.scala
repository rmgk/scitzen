package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}

import better.files.File
import scitzen.generic.{Article, DocumentDirectory}
import scitzen.outputs.SastToScimConverter
import scitzen.sast.Sast

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
          scribe.info(
            s"could not format ${parsed.file}, did not contain a single article with a date"
          )
    }

  def formatContent(file: File, originalContent: String, sast: Seq[Sast]): Unit =
    val result    = SastToScimConverter.toScimS(sast)
    val resultStr = result.iterator.mkString("", "\n", "\n")
    if resultStr != originalContent then
      scribe.info(s"formatting ${file.name}")
      file.write(resultStr)

  def renameFileFromHeader(f: File, sdoc: Article): Unit =
    val newName: String = canonicalName(sdoc) + ".scim"

    if newName != f.name then
      scribe.info(s"rename ${f.name} to $newName")
      f.renameTo(newName)

  def canonicalName(header: Article): String =
    val title = sluggify(header.title)
    header.date.map(_.date.full).fold(title)(d => d + " " + title)

  def sluggify(str: String): String =
    str
      .replaceAll("""[<>":;%/\?\[\]\\\*\|]""", "-")
      .replaceAll("\\s+", " ")
      .replaceAll("-+", "-")
      .trim
      .replaceAll("^-|-$", "")
      .trim

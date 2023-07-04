package scitzen.cli

import scitzen.bibliography.BibDB

import java.nio.charset.{Charset, StandardCharsets}
import scitzen.generic.ArticleDirectory
import scitzen.outputs.SastToScimConverter
import scitzen.sast.{Sast, Section}
import scitzen.compat.Logging.cli

import java.nio.file.{Files, Path}

object Format:

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8

  def formatContents(ca: ConversionAnalysis): Unit =
    ca.directory.byPath.foreach { (path, articles) =>
      formatContent(path.absolute, articles.head.doc.content, articles.flatMap(_.sast), ca.bib)
    }

  def formatRename(documentDirectory: ArticleDirectory): Unit =
    documentDirectory.byPath.foreach: (document, articles) =>
      articles match
        case Seq(article) =>
          article.titled match
            case Some(t) if t.date.isDefined => renameFileFromHeader(document.absolute, t)
            case _ => cli.trace(
                s"could not format ${document.absolute}, did not contain a single article with a date"
              )
        case _ =>
          cli.trace(
            s"could not format ${document.absolute}, did not contain a single article with a date"
          )

  def formatContent(file: Path, originalContent: Array[Byte], sast: Seq[Sast], bibDB: BibDB): Unit =
    val result      = SastToScimConverter(bibDB).toScimS(sast)
    val resultBytes = result.iterator.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8)
    if !java.util.Arrays.equals(resultBytes, originalContent) then
      cli.info(s"formatting ${file.getFileName}")
      Files.write(file, resultBytes)
      ()

  def renameFileFromHeader(f: Path, sdoc: Section): Unit =
    val newName: String = canonicalName(sdoc) + ".scim"

    if newName != f.getFileName.toString then
      cli.info(s"rename ${f.getFileName} to $newName")
      Files.move(f, f.resolveSibling(newName))
      ()

  def canonicalName(header: Section): String =
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

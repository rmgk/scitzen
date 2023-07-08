package scitzen.cli

import scitzen.bibliography.BibDB

import java.nio.charset.{Charset, StandardCharsets}
import scitzen.generic.ArticleDirectory
import scitzen.outputs.SastToScimConverter
import scitzen.sast.{Sast, Section}
import scitzen.compat.Logging.cli

import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Path, StandardOpenOption}

object Format:

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8

  def formatContents(ca: ConversionAnalysis): Unit =
    val cachefile = ca.project.cacheDir.resolve("formatted")
    var formattedHashes =
      if !Files.exists(cachefile)
      then Map.empty
      else
        Files.lines(cachefile, StandardCharsets.UTF_8).iterator().asScala.map: v =>
          val Array(date, file) = v.split("\t", 2)
          (Path.of(file), date)
        .toMap
    ca.directory.byPath.foreach: (path, articles) =>
      val content = articles.head.doc.content
      val modified = Files.getLastModifiedTime(path.absolute)
      if !formattedHashes.get(path.absolute).contains(modified.toString)
      then
        formatContent(path.absolute, content, articles.flatMap(_.sast), ca.bib)
        formattedHashes = formattedHashes.updated(path.absolute, Files.getLastModifiedTime(path.absolute).toString)
    Files.createDirectories(cachefile.getParent)
    Files.write(
      cachefile,
      formattedHashes.iterator.map((p, t) => s"$t\t$p").mkString("\n").getBytes(StandardCharsets.UTF_8),
      StandardOpenOption.TRUNCATE_EXISTING,
      StandardOpenOption.CREATE
    )
    ()

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

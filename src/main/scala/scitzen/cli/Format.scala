package scitzen.cli

import scitzen.bibliography.BibDB
import scitzen.compat.Logging.cli
import scitzen.project.{ArticleDirectory, ProjectPath}
import scitzen.outputs.SastToScimConverter
import scitzen.sast.{Sast, Section}

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.jdk.CollectionConverters.*
import scitzen.compat.Logging.given

object Format:

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8

  def formatContents(ca: ConversionAnalysis): Unit =
    val cachefile = ca.project.cacheDir.resolve("formatted.tsv")
    var formattedHashes =
      if !Files.exists(cachefile)
      then Map.empty
      else
        Files.lines(cachefile, StandardCharsets.UTF_8).iterator().asScala.map: v =>
          val Array(date, file) = v.split("\t", 2)
          (Path.of(file), date)
        .toMap
    ca.directory.byPath.foreach: (path, articles) =>
      val content  = articles.head.doc.content
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

  def formatRename(documentDirectory: ArticleDirectory, selected: List[ProjectPath], bibDB: BibDB): Unit =
    documentDirectory.byPath.foreach: (document, articles) =>
      if !selected.contains(document)
      then ()
      else
        articles.toList match
          case Seq(article) =>
            article.titled match
              case Some(t) => renameFileFromHeader(document.absolute, t)
              case _ => cli.trace(
                  s"could not format ${document.absolute}, did not contain a titled article"
                )
          case head :: _ =>
            val remaining = articles.flatMap: art =>
              art.titled match
                case Some(title) =>
                  val target = art.doc.path.absolute.resolveSibling(canonicalName(title, ".scim"))
                  if Files.exists(target)
                  then
                    cli.warn(s"rename target exists")
                    art.sast
                  else
                    formatContent(target, Array.emptyByteArray, art.sast, bibDB)
                    Nil
                case None =>
                  art.sast
            if remaining.isEmpty
            then Files.delete(head.doc.path.absolute)
            else formatContent(head.doc.path.absolute, head.doc.content, remaining, bibDB)

          case other =>
            cli.warn(
              s"could not format ${document.absolute}, did not contain anything???",
            )

  def formatContent(file: Path, originalContent: Array[Byte], sast: Seq[Sast], bibDB: BibDB): Unit =
    val result      = SastToScimConverter(bibDB).toScimS(sast)
    val resultBytes = result.iterator.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8)
    if !java.util.Arrays.equals(resultBytes, originalContent) then
      cli.info(s"formatting ${file.getFileName}")
      Files.write(file, resultBytes)
      ()

  def renameFileFromHeader(orig: Path, sdoc: Section): Unit =
    val newName: String = canonicalName(sdoc, ".scim")
    val target          = orig.resolveSibling(newName)
    if newName != orig.getFileName.toString then
      if Files.exists(target)
      then cli.warn(s"rename target already exists", orig)
      else
        cli.info(s"rename ${orig.getFileName} to $newName")
        Files.move(orig, target)
        ()

  def canonicalName(header: Section, extension: String): String =
    val proto      = if extension == ".scim" then header.title else header.filename.getOrElse(header.title)
    val title      = sluggify(proto)
    val shortTitle = title.substring(0, math.min(160, title.length))
    val name       = header.date.map(_.date.full).fold(shortTitle)(d => d + " " + shortTitle)
    name.stripSuffix(".") + extension

  def sluggify(str: String): String =
    str
      .replaceAll("""[<>":;%/\?\[\]\\\*\|]""", "-")
      .replaceAll("\\s+", " ")
      .replaceAll("-+", "-")
      .trim
      .replaceAll("^-|-$", "")
      .trim

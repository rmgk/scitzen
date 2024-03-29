package scitzen.resources

import java.nio.file.Path

object Filetype:
  val all: Set[Filetype]            = Set(svg, png, pdf, webp, jpg, mp4, gif)
  val lookup: Map[String, Filetype] = all.flatMap(ft => (ft.extension, ft) +: ft.aliases.map(al => (al, ft))).toMap

  def nameWithoutExtension(p: Path): String =
    val filename = p.getFileName.toString
    val ext      = filename.lastIndexOf('.')
    if ext >= 0
    then filename.substring(0, ext)
    else filename

  def of(p: Path): Option[Filetype] =
    val filename = p.getFileName.toString
    val ext      = filename.lastIndexOf('.')
    if ext >= 0
    then Filetype.lookup.get(filename.substring(ext + 1, filename.length))
    else None

/** like a mime type, but worse */
enum Filetype(val extension: String, val aliases: String*):
  case svg  extends Filetype("svg")
  case png  extends Filetype("png")
  case pdf  extends Filetype("pdf")
  case webp extends Filetype("webp")
  case jpg  extends Filetype("jpg", "jpeg")
  case mp4  extends Filetype("mp4")
  case gif  extends Filetype("gif")

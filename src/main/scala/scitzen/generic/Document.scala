package scitzen.generic

import scitzen.sast.{Block, Directive, Prov}

import java.nio.file.{Files, Path}
import scala.collection.immutable.ArraySeq

/** A document represents a single, on disk, text file */
case class Document(path: ProjectPath, content: Array[Byte]):
  val uid: String                             = Integer.toHexString(path.hashCode())
  lazy val reporter: FileReporter             = new FileReporter(path, content)
  def resolve(p: String): Option[ProjectPath] = path.project.resolve(path.directory, p)

object Document:
  def apply(file: ProjectPath): Document =
    val content = Files.readAllBytes(file.absolute)
    Document(file, content)

trait Reporter:
  def apply(im: Directive): String = apply(im.prov)
  def apply(im: Block): String     = apply(im.prov)
  def apply(prov: Prov): String

final class FileReporter(file: ProjectPath, content: Array[Byte]) extends Reporter:
  lazy val newLines: Seq[Int] =
    def findNL(idx: Int, found: List[Int]): Array[Int] =
      val res = content.indexOf('\n', idx + 1)
      if res >= 0 then findNL(res, res :: found)
      else found.toArray.reverse

    ArraySeq.unsafeWrapArray(findNL(-1, Nil))

  def indexToPosition(idx: Int): (Int, Int) =
    val ip     = newLines.search(idx).insertionPoint
    val offset = if ip == 0 then 0 else newLines(ip - 1)
    (ip + 1, idx - offset)

  override def apply(prov: Prov): String =
    val pos = indexToPosition(prov.start)
    s" at »${Path.of("").toAbsolutePath.relativize(file.absolute)}:" +
    s"${pos._1}:${pos._2}«"

  // macro offsets are in bytes, but sublime expects them to be in codepoints, so we adapt
  lazy val byteOffsets: Array[Int] = content.iterator.zipWithIndex.collect {
    case (b, pos) if (b & (192)) == 128 => pos
  }.toArray
  def bytePosToCodepointPos(v: Int) = v - byteOffsets.count(_ <= v)

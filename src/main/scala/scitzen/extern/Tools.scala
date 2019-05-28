package scitzen.extern

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import better.files.File

import scala.sys.process.Process


object Hashes {
  private val sha1digester: MessageDigest = MessageDigest.getInstance("SHA-1")
  def sha1(b: Array[Byte]): Array[Byte] = sha1digester.clone().asInstanceOf[MessageDigest].digest(b)
  def sha1hex(b: Array[Byte]): String = sha1(b).map { h => f"$h%02x" }.mkString
  def sha1hex(s: String): String = sha1hex(s.getBytes(StandardCharsets.UTF_8))
}

object ImageConvert {
  def pdfToSvg(in: File): File = {
    val out = in.sibling(in.name + ".svg")
    if (out.exists) return out
    Process(List("inkscape", in.pathAsString, "--export-plain-svg", out.pathAsString)).!
    out
  }
}
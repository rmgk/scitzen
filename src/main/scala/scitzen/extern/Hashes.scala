package scitzen.extern

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object Hashes {
  private val sha1digester: MessageDigest = MessageDigest.getInstance("SHA-1")
  def sha1(b: Array[Byte]): Array[Byte]   = sha1digester.clone().asInstanceOf[MessageDigest].digest(b)
  def sha1hex(b: Array[Byte]): String     = sha1(b).map { h => f"$h%02x" }.mkString
  def sha1hex(s: String): String          = sha1hex(s.getBytes(StandardCharsets.UTF_8))
}

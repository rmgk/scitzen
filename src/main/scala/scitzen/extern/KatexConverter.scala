package scitzen.extern

import java.lang.ProcessBuilder.Redirect

import better.files._

case class KatexConverter(cache: Map[String, String], katexdefs: Option[File]):

  def convert(str: String): (String, Option[KatexConverter]) =
    cache.get(str) match
      case Some(res) => (res, None)
      case None =>
        val pb = katexdefs match
          case None       => new ProcessBuilder("katex")
          case Some(file) => new ProcessBuilder("katex", "--macro-file", file.pathAsString)
        val process = pb.redirectInput(Redirect.PIPE)
          .redirectOutput(Redirect.PIPE)
          .start()
        process.getOutputStream.writeAndClose(str)
        process.waitFor()
        val res = process.getInputStream.asString()
        (res, Some(copy(cache = cache.updated(str, res))))

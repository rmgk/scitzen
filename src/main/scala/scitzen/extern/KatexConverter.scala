package scitzen.extern

import java.lang.ProcessBuilder.Redirect

import better.files._

case class KatexConverter(katexdefs: Option[File]) {

  def convert(str: String): String = {
    val pb = katexdefs match {
      case None       => new ProcessBuilder("katex")
      case Some(file) => new ProcessBuilder("katex", "--macro-file", file.pathAsString)
    }
    val process = pb.redirectInput(Redirect.PIPE)
                    .redirectOutput(Redirect.PIPE)
                    .start()
    process.getOutputStream.writeAndClose(str)
    process.waitFor()
    process.getInputStream.asString()
  }

}

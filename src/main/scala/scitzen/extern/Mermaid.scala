package scitzen.extern

import java.nio.charset.StandardCharsets

import better.files.File

object Mermaid {
  def convert(content: String, working: File, format: String): ConvertSchedulable[File] = {
    val bytes         = content.getBytes(StandardCharsets.UTF_8)
    val hash          = Hashes.sha1hex(bytes)
    val dir           = working / hash
    val target        = dir / (hash + s".$format")
    val mermaidSource = dir / (hash + ".mermaid")
    new ConvertSchedulable(
      target,
      if (target.exists) None
      else Some(new ConvertTask {
        override def run(): Unit = {

          val start = System.nanoTime()

          mermaidSource.createIfNotExists(createParents = true)
          mermaidSource.writeByteArray(bytes)

          new ProcessBuilder("mmdc",
                             "--input", mermaidSource.pathAsString,
                             "--output", target.pathAsString)
          .inheritIO().start().waitFor()
          scribe.info(s"mermaid compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
        }
      }))
  }
}

package scitzen.extern

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import better.files.File

import scala.sys.process.Process


object Graphviz {
  def convert(content: String, working: File, layout: String, format: String): (String, File) = {
    val bytes = (s"//$layout\n" + content).getBytes(StandardCharsets.UTF_8)
    val hash = Hashes.sha1hex(bytes)
    val dir = working / hash
    val target = dir / (hash + s".$format")
    if (target.exists) return hash -> target

    dir.createDirectories()

    val start = System.nanoTime()
    val command = List("dot",
                  s"-K$layout",
                  s"-T$format",
                  s"-o${target.pathAsString}")
    (Process(command) #<
     new ByteArrayInputStream(bytes)).!
    scribe.info(s"graphviz compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    hash -> target
  }

}

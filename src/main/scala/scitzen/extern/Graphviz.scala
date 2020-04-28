package scitzen.extern

import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets

import better.files._

object Graphviz {
  def convert(content: String, working: File, layout: String, format: String): ConvertSchedulable[File] = {
    val bytes  = (s"//$layout\n" + content).getBytes(StandardCharsets.UTF_8)
    val hash   = Hashes.sha1hex(bytes)
    val dir    = working / hash
    val target = dir / (hash + s".$format")
    new ConvertSchedulable(
      target,
      if (target.exists) None
      else Some(new ConvertTask {
        override def run(): Unit = {
          dir.createDirectories()

          val start   = System.nanoTime()
          val process = new ProcessBuilder("dot",
                                           s"-K$layout",
                                           s"-T$format",
                                           s"-o${target.pathAsString}")
          .inheritIO().redirectInput(Redirect.PIPE).start()
          process.getOutputStream.autoClosed.foreach {_.write(bytes)}
          process.waitFor()
          scribe.info(s"graphviz compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
        }
      }))
  }

}

package scitzen.blockconverters

import scitzen.compat.Logging.cli
import scitzen.extern.Hashes
import scitzen.sast.{Attributes, DCommand, Directive, Sast}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

object MermaidModule extends BlockConverterModule {
  override def handles: String = "mermaid"
  override def convert(converterParams: ConverterParams): List[Sast] =
    import converterParams.*
    val bytes         = content.getBytes(StandardCharsets.UTF_8)
    val name          = Hashes.sha1hex(bytes)
    val format        = "svg"
    val target        = project.cachePath(Path.of(s"$name/$name.$format"))
    val mermaidSource = project.cachePath(Path.of(s"$name/$name.mermaid"))
    if !Files.exists(target.absolute) then
      val start = System.nanoTime()

      Files.createDirectories(mermaidSource.absolute.getParent)
      Files.write(mermaidSource.absolute, bytes)

      new ProcessBuilder(
        "npx",
        "@mermaid-js/mermaid-cli",
        "--input",
        mermaidSource.absolute.toString,
        "--output",
        target.absolute.toString
      ).inheritIO().start().waitFor()
      cli.info(s"mermaid compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    List(Directive(DCommand.Image, Attributes.target(target.projectAbsolute.toString))(block.prov))
}

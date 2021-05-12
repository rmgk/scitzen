package scitzen.extern

import better.files.{File, _}
import scitzen.generic.RegexContext.regexStringContext
import scitzen.generic.{DocumentDirectory, Project}
import scitzen.outputs.{Includes, SastToTextConverter}
import scitzen.parser.Parse
import scitzen.sast.{Attributes, Block, Fenced, Prov}

import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.attribute.FileTime
import scala.jdk.CollectionConverters._

class ImageConverter(
    project: Project,
    val preferredFormat: String,
    unsupportedFormat: List[String] = Nil,
    documentDirectory: DocumentDirectory
) {

  def requiresConversion(filename: String): Boolean =
    unsupportedFormat.exists(fmt => filename.endsWith(fmt))

  def applyConversion(file: File): File = {
    preferredFormat match {
      case "svg" | "png" if (file.extension.contains(".pdf")) => pdfToCairo(file)
      case "pdf" | "png" if (file.extension.contains(".svg")) => svgToCairo(file)
    }
  }

  def pdfToCairo(file: File): File = {
    convertExternal(
      file,
      (source, target) => {
        preferredFormat match {
          case "png" | "jpeg" | "tiff" =>
            List(
              "pdftocairo",
              "-singlefile",
              s"-$preferredFormat",
              source.pathAsString,
              target.sibling(target.nameWithoutExtension).pathAsString
            )

          case "svg" =>
            List("pdftocairo", s"-$preferredFormat", source.pathAsString, target.pathAsString)
        }
      }
    )
  }

  def svgToCairo(file: File): File = {
    convertExternal(
      file,
      (source, target) => {
        List("cairosvg", source.pathAsString, "-o", target.pathAsString)
      }
    )
  }

  trait CommandFunction {
    def genCommand(source: File, target: File): List[String]
  }

  private def convertExternal(file: File, command: CommandFunction): File = {
    val relative = project.root.relativize(file.parent)
    val targetfile = File((project.cacheDir / "convertedImages")
      .path.resolve(relative)
      .resolve(file.nameWithoutExtension + s".$preferredFormat"))
    val sourceModified = file.lastModifiedTime
    if (!targetfile.exists || targetfile.lastModifiedTime != sourceModified) {
      targetfile.parent.createDirectories()
      scribe.debug(s"converting $file to $targetfile")
      new ProcessBuilder(command.genCommand(file, targetfile).asJava)
        .inheritIO().start().waitFor()
      Files.setLastModifiedTime(targetfile.path, FileTime.from(file.lastModifiedTime))
    }
    targetfile
  }

  def convertBlock(cwd: File, tlb: Block): Option[File] = {
    val converter = tlb.attributes.named("converter")
    val content   = tlb.content.asInstanceOf[Fenced].content
    convertString(converter, tlb.attributes, tlb.prov, content, cwd)
  }

  def convertString(
      converter: String,
      attributes: Attributes,
      prov: Prov,
      content: String,
      cwd: File
  ): Option[File] = {

    val templatedContent = attributes.named.get("template").flatMap(project.resolve(cwd, _)) match {
      case None => content
      case Some(templateFile) =>
        val tc   = templateFile.contentAsString
        val sast = Parse.documentUnwrap(tc, Prov(0, tc.length))
        SastToTextConverter(
          project.config.definitions ++ attributes.named + (
            "template content" -> content
          ),
          Some(Includes(project, templateFile, documentDirectory))
        ).convert(sast).mkString("\n")
    }

    converter match {
      case "tex" =>
        val pdffile = texconvert(templatedContent, project.cacheDir)
        if (preferredFormat == "svg" || preferredFormat == "png") Some(pdfToCairo(pdffile))
        else Some(pdffile)

      case gr @ rex"graphviz.*" =>
        Some(graphviz(templatedContent, project.cacheDir, gr.split("\\s+", 2).lift(1), preferredFormat))
      case rex"mermaid" =>
        Some(mermaid(templatedContent, project.cacheDir, preferredFormat))
      case other =>
        scribe.warn(s"unknown converter $other")
        None
    }
  }

  def graphviz(content: String, working: File, layout: Option[String], format: String): File = {
    val bytes  = (s"//$layout\n" + content).getBytes(StandardCharsets.UTF_8)
    val hash   = Hashes.sha1hex(bytes)
    val dir    = working / hash
    val target = dir / (hash + s".$format")
    if (!target.exists) {
      dir.createDirectories()

      val start = System.nanoTime()
      val process = new ProcessBuilder(
        "dot",
        layout.map(l => s"-K$l").getOrElse("-Kdot"),
        s"-T$format",
        s"-o${target.pathAsString}"
      )
        .inheritIO().redirectInput(Redirect.PIPE).start()
      process.getOutputStream.autoClosed.foreach { _.write(bytes) }
      process.waitFor()
      scribe.info(s"graphviz compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    }
    target
  }

  def mermaid(content: String, working: File, format: String): File = {
    val bytes         = content.getBytes(StandardCharsets.UTF_8)
    val hash          = Hashes.sha1hex(bytes)
    val dir           = working / hash
    val target        = dir / (hash + s".$format")
    val mermaidSource = dir / (hash + ".mermaid")
    if (!target.exists) {
      val start = System.nanoTime()

      mermaidSource.createIfNotExists(createParents = true)
      mermaidSource.writeByteArray(bytes)

      new ProcessBuilder("mmdc", "--input", mermaidSource.pathAsString, "--output", target.pathAsString)
        .inheritIO().start().waitFor()
      scribe.info(s"mermaid compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    }
    target
  }

  def texconvert(content: String, working: File): File = {
    val hash   = Hashes.sha1hex(content)
    val dir    = working / hash
    val target = dir / (hash + ".pdf")
    if (target.exists) target
    else {
      val texbytes = content.getBytes(StandardCharsets.UTF_8)
      val dir      = target.parent
      dir.createDirectories()
      val texfile = dir / (hash + ".tex")
      texfile.writeByteArray(texbytes)
      Latexmk.latexmk(dir, hash, texfile)
    }
    target
  }

}

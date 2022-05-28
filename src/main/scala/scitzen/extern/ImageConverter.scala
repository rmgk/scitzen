package scitzen.extern

import better.files.{File, *}
import scitzen.compat.Logging.scribe
import scitzen.generic.{DocumentDirectory, PreprocessedResults, Project}
import scitzen.outputs.{Includes, SastToTextConverter}
import scitzen.sast.{Attributes, Block, Fenced, Prov}
import scitzen.scipparse.Parse

import java.io.IOError
import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.attribute.FileTime
import scala.jdk.CollectionConverters.*

object ImageConverter {

  def preprocessImages(
      project: Project,
      documentDirectory: DocumentDirectory,
      targets: List[ImageTarget] = Nil,
      preprocessed: PreprocessedResults
  ): Unit =
    val converters = targets.map(t => t -> new ImageConverter(project, t, documentDirectory)).toMap
    val blocks = preprocessed.docCtx.flatMap { (doc, ctx) =>
      ctx.convertBlocks.map(b => b.attributes -> (b -> doc))
    }.toMap

    blocks.valuesIterator.foreach { (block, doc) =>
      converters.foreach { (target, ic) =>
        ic.convertBlock(doc.file, block)
      }
    }

    val macros = preprocessed.docCtx.flatMap { (doc, ctx) =>
      ctx.imageMacros.map(m => m.attributes -> (m -> doc))
    }.toMap

    macros.valuesIterator.foreach { (mcro, doc) =>
      val file = project.resolve(doc.file.parent, mcro.attributes.target)
      converters.foreach { (t, ic) =>
        if t.requiresConversion(mcro.attributes.target) && file.isDefined then
          converters(t).applyConversion(file.get, mcro.attributes).map(f => (t -> f))
      }
    }
  end preprocessImages

  def applyTemplate(attributes: Attributes, content: String, cwd: File, project: Project, documentDirectory: DocumentDirectory): String =
    val templatedContent = attributes.named.get("template").flatMap(project.resolve(cwd, _)) match
      case None => content
      case Some(templateFile) =>
        val tc   = templateFile.byteArray
        val sast = Parse.documentUnwrap(tc, Prov(0, tc.length))
        SastToTextConverter(
          project.config.definitions ++ attributes.named + (
            "template content" -> content
            ),
          Some(Includes(project, templateFile, documentDirectory))
          ).convert(sast).mkString("\n")
    templatedContent
}

class ImageConverter(
    project: Project,
    imageTarget: ImageTarget,
    documentDirectory: DocumentDirectory,
):

  def preferredFormat: String         = imageTarget.preferredFormat
  def unsupportedFormat: List[String] = imageTarget.unsupportedFormat

  def requiresConversion(filename: String): Boolean =
    unsupportedFormat.exists(fmt => filename.endsWith(fmt))

  def applyConversion(file: File, attributes: Attributes): Option[File] =
    preferredFormat match
      case "svg" | "png" if (file.extension.contains(".pdf")) => Some(pdfToCairo(file))
      case "pdf" | "png" if (file.extension.contains(".svg")) => Some(svgToCairo(file))
      case _ if (file.extension.contains(".tex")) =>
        val dir       = (project.cacheDir / "convertedImages").path.resolve(project.root.relativize(file))
        val templated = ImageConverter.applyTemplate(attributes, file.contentAsString, file.parent, project, documentDirectory)
        convertTemplated("tex", templated, dir, file.nameWithoutExtension(includeAll = false))

  def pdfToCairo(file: File): File =
    convertExternal(
      file,
      (source, target) =>
        preferredFormat match
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
    )

  def svgToCairo(file: File): File =
    convertExternal(
      file,
      (source, target) => {
        List("cairosvg", source.pathAsString, "-o", target.pathAsString)
      }
    )

  trait CommandFunction:
    def genCommand(source: File, target: File): List[String]

  private def convertExternal(file: File, command: CommandFunction): File =
    val relative = project.root.relativize(file)
    val targetfile =
      if project.cacheDir.isParentOf(file) then
        file.sibling(file.nameWithoutExtension(includeAll = false) + s".$preferredFormat")
      else
        File((project.cacheDir / "convertedImages")
          .path.resolve(relative)
          .resolve(file.nameWithoutExtension + s".$preferredFormat"))
    val sourceModified = file.lastModifiedTime
    if !targetfile.exists || targetfile.lastModifiedTime != sourceModified then
      targetfile.parent.createDirectories()
      scribe.debug(s"converting $file to $targetfile")
      new ProcessBuilder(command.genCommand(file, targetfile).asJava)
        .inheritIO().start().waitFor()
      Files.setLastModifiedTime(targetfile.path, FileTime.from(file.lastModifiedTime))
    targetfile

  def convertBlock(cwd: File, tlb: Block): Option[File] =
    val converter = tlb.attributes.named("converter")
    val content   = tlb.content.asInstanceOf[Fenced].content
    val templated = ImageConverter.applyTemplate(tlb.attributes, content, cwd, project, documentDirectory)
    val hash      = tlb.attributes.named("content hash")
    convertTemplated(converter, templated, project.cacheDir / hash, hash)

  def convertTemplated(
      converter: String,
      templatedContent: String,
      dir: File,
      name: String,
  ): Option[File] =
    try
      converter match
        case "tex" =>
          val pdffile = texconvert(templatedContent, dir, name)
          if preferredFormat == "svg" || preferredFormat == "png" then Some(pdfToCairo(pdffile))
          else Some(pdffile)
        case gr @ "graphviz" =>
          Some(graphviz(templatedContent, dir, name, preferredFormat))
        case "mermaid" =>
          Some(mermaid(templatedContent, dir, name, preferredFormat))
        case other =>
          scribe.warn(s"unknown converter $other")
          None
    catch
      case e: java.io.IOException =>
        scribe.warn(s"converter $converter failed with: »${e.getMessage}«")
        None

  def graphviz(content: String, dir: File, name: String, format: String): File =
    val bytes  = content.getBytes(StandardCharsets.UTF_8)
    val target = dir / (name + s".$format")
    if !target.exists then
      dir.createDirectories()

      val start = System.nanoTime()
      val process = new ProcessBuilder(
        "dot",
        s"-T$format",
        s"-o${target.pathAsString}"
      )
        .inheritIO().redirectInput(Redirect.PIPE).start()
      process.getOutputStream.autoClosed.foreach { _.write(bytes) }
      process.waitFor()
      scribe.info(s"graphviz compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    target

  def mermaid(content: String, dir: File, name: String, format: String): File =
    val bytes         = content.getBytes(StandardCharsets.UTF_8)
    val target        = dir / (name + s".$format")
    val mermaidSource = dir / (name + ".mermaid")
    if !target.exists then
      val start = System.nanoTime()

      mermaidSource.createIfNotExists(createParents = true)
      mermaidSource.writeByteArray(bytes)

      new ProcessBuilder("mmdc", "--input", mermaidSource.pathAsString, "--output", target.pathAsString)
        .inheritIO().start().waitFor()
      scribe.info(s"mermaid compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    target

  def texconvert(content: String, dir: File, name: String): File =
    val target = dir / (name + ".pdf")
    if target.exists then target
    else
      val texbytes = content.getBytes(StandardCharsets.UTF_8)
      val dir      = target.parent
      dir.createDirectories()
      val texfile = dir / (name + ".tex")
      texfile.writeByteArray(texbytes)
      Latexmk.latexmk(dir, name, texfile)
    target

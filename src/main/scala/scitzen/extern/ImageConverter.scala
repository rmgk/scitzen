package scitzen.extern

import scitzen.compat.Logging.scribe
import scitzen.generic.{ArticleDirectory, Project}
import scitzen.outputs.SastToTextConverter
import scitzen.sast.{Attributes, Block, Fenced}

import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

object ImageConverter {

  def nameWithoutExtension(p: Path): String = p.getFileName.toString.reverse.dropWhile(c => c != '.').drop(1).reverse

  def preprocessImages(
      project: Project,
      targets: List[ImageTarget] = Nil,
      articleDirectory: ArticleDirectory
  ): Unit =
    val converters = targets.map(t => t -> new ImageConverter(project, t, articleDirectory)).toMap
    val blocks = articleDirectory.articles.flatMap { art =>
      art.context.convertBlocks.map(b => b.attributes -> (b -> art.sourceDoc))
    }.toMap

    blocks.valuesIterator.foreach { (block, doc) =>
      converters.foreach { (_, ic) =>
        ic.convertBlock(doc.path.directory, block)
      }
    }

    val macros = articleDirectory.articles.flatMap { art =>
      art.context.imageMacros.map(m => m.attributes -> (m -> art.sourceDoc))
    }.toMap

    macros.valuesIterator.foreach { (mcro, doc) =>
      val file = project.resolve(doc.path.directory, mcro.attributes.target)
      converters.foreach { (t, _) =>
        if t.requiresConversion(mcro.attributes.target) && file.isDefined then
          converters(t).applyConversion(file.get.absolute, mcro.attributes).map(f => (t -> f))
          ()
      }
    }
  end preprocessImages

  def applyTemplate(
      attributes: Attributes,
      content: String,
      cwd: Path,
      project: Project,
      articleDirectory: ArticleDirectory
  ): String =
    attributes.named.get("template") match
      case None =>
        scribe.error(s"no template")
        content
      case Some(pathString) => project.resolve(cwd, pathString) match
          case None =>
            scribe.error(s"could not resolve $pathString")
            content
          case Some(templatePath) =>
            articleDirectory.byPath.get(templatePath) match
              case None =>
                scribe.error(s"not resolved $templatePath")
                content
              case Some(articles) =>
                val sast = articles.flatMap(_.content)
                SastToTextConverter(
                  project.config.definitions ++ attributes.named + (
                    "template content" -> content
                  ),
                  articleDirectory
                ).convert(sast).mkString("\n")
}

class ImageConverter(
    project: Project,
    imageTarget: ImageTarget,
    documentDirectory: ArticleDirectory,
):

  def preferredFormat: String         = imageTarget.preferredFormat
  def unsupportedFormat: List[String] = imageTarget.unsupportedFormat

  def requiresConversion(filename: String): Boolean =
    unsupportedFormat.exists(fmt => filename.endsWith(fmt))

  def applyConversion(file: Path, attributes: Attributes): Option[Path] =
    preferredFormat match
      case "svg" | "png" if (file.getFileName.toString.endsWith(".pdf")) => Some(pdfToCairo(file))
      case "pdf" | "png" if (file.getFileName.toString.endsWith(".svg")) => Some(svgToCairo(file))
      case _ if (file.getFileName.toString.endsWith(".tex")) =>
        val dir = project.cacheDir.resolve("convertedImages").resolve(project.root.relativize(file))
        val templated =
          ImageConverter.applyTemplate(attributes, Files.readString(file), file.getParent, project, documentDirectory)
        convertTemplated("tex", templated, dir, ImageConverter.nameWithoutExtension(file))

  def pdfToCairo(file: Path): Path =
    convertExternal(
      file,
      (source, target) =>
        preferredFormat match
          case "png" | "jpeg" | "tiff" =>
            List(
              "pdftocairo",
              "-singlefile",
              s"-$preferredFormat",
              source.toAbsolutePath.toString,
              target.resolveSibling(ImageConverter.nameWithoutExtension(target)).toAbsolutePath.toString
            )

          case "svg" =>
            List("pdftocairo", s"-$preferredFormat", source.toAbsolutePath.toString, target.toAbsolutePath.toString)
    )

  def svgToCairo(file: Path): Path =
    convertExternal(
      file,
      (source, target) => {
        List("cairosvg", source.toAbsolutePath.toString, "-o", target.toAbsolutePath.toString)
      }
    )

  trait CommandFunction:
    def genCommand(source: Path, target: Path): List[String]

  private def convertExternal(file: Path, command: CommandFunction): Path =
    val relative       = project.root.relativize(file)
    val targetfileName = ImageConverter.nameWithoutExtension(file) + s".$preferredFormat"
    val targetfile =
      if file.startsWith(project.cacheDir) then
        file.resolveSibling(targetfileName)
      else
        project.cacheDir.resolve("convertedImages")
          .resolve(relative)
          .resolve(targetfileName)
    val sourceModified = Files.getLastModifiedTime(file)
    if !Files.exists(targetfile) || Files.getLastModifiedTime(targetfile) != sourceModified then
      Files.createDirectories(targetfile.getParent)
      scribe.debug(s"converting $file to $targetfile")
      new ProcessBuilder(command.genCommand(file, targetfile).asJava)
        .inheritIO().start().waitFor()
      Files.setLastModifiedTime(targetfile, Files.getLastModifiedTime(file))
      ()
    targetfile

  def convertBlock(cwd: Path, tlb: Block): Option[Path] =
    tlb.attributes.named.get("converter") match
      case Some(converter) =>
        val content   = tlb.content.asInstanceOf[Fenced].content
        val templated = ImageConverter.applyTemplate(tlb.attributes, content, cwd, project, documentDirectory)
        val hash      = tlb.attributes.named("content hash")
        convertTemplated(converter, templated, project.cacheDir.resolve(hash), hash)
      case None => None

  def convertTemplated(
      converter: String,
      templatedContent: String,
      dir: Path,
      name: String,
  ): Option[Path] =
    try
      converter match
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

  def graphviz(content: String, dir: Path, name: String, format: String): Path =
    val bytes  = content.getBytes(StandardCharsets.UTF_8)
    val target = dir.resolve(name + s".$format")
    if !Files.exists(target) then
      Files.createDirectories(dir)

      val start = System.nanoTime()
      val process = new ProcessBuilder(
        "dot",
        s"-T$format",
        s"-o${target.toAbsolutePath.toString}"
      )
        .inheritIO().redirectInput(Redirect.PIPE).start()
      Using.resource(process.getOutputStream) { os => os.write(bytes) }
      process.waitFor()
      scribe.info(s"graphviz compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    target

  def mermaid(content: String, dir: Path, name: String, format: String): Path =
    val bytes         = content.getBytes(StandardCharsets.UTF_8)
    val target        = dir.resolve(name + s".$format")
    val mermaidSource = dir.resolve(name + ".mermaid")
    if !Files.exists(target) then
      val start = System.nanoTime()

      Files.createDirectories(mermaidSource.getParent)
      Files.write(mermaidSource, bytes)

      new ProcessBuilder(
        "mmdc",
        "--input",
        mermaidSource.toAbsolutePath.toString,
        "--output",
        target.toAbsolutePath.toString
      )
        .inheritIO().start().waitFor()
      scribe.info(s"mermaid compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    target

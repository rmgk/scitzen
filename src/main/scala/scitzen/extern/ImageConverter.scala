package scitzen.extern

import better.files.{File, _}
import scitzen.extern.ImageTarget.Undetermined
import scitzen.generic.RegexContext.regexStringContext
import scitzen.generic.{DocumentDirectory, PreprocessedResults, Project}
import scitzen.outputs.{Includes, SastToTextConverter}
import scitzen.parser.Parse
import scitzen.sast.{Attributes, Block, Fenced, Prov}
import cats.kernel.{Monoid, Semigroup}
import cats.instances.map.catsKernelStdMonoidForMap

import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.attribute.FileTime
import scala.jdk.CollectionConverters._

case class ImageSubstitutions(mapping: Map[Attributes, Map[ImageTarget, File]]) {
  def get(attr: Attributes, target: ImageTarget = Undetermined): Option[File] = {
    mapping.get(attr).orElse {
      scribe.warn(s"could not find $attr in converted files")
      None
    }.flatMap(_.get(target).orElse {
      scribe.warn(s"could not find conversion for $target")
      None
    })
  }
}

sealed class ImageTarget(val preferredFormat: String, val unsupportedFormat: List[String]) {
  def requiresConversion(filename: String): Boolean = unsupportedFormat.exists(fmt => filename.endsWith(fmt))
}
object ImageTarget {
  object Undetermined extends ImageTarget("", Nil)
  object Html         extends ImageTarget("svg", List("pdf"))
  object Tex          extends ImageTarget("pdf", List("svg"))
  object Raster       extends ImageTarget("png", List("svg", "pdf"))
}

object ImageConverter {
  def preprocessImages(
      project: Project,
      documentDirectory: DocumentDirectory,
      targets: List[ImageTarget] = Nil,
      preprocessed: PreprocessedResults
  ): ImageSubstitutions = {
    val converters = targets.map(t => t -> new ImageConverter(project, t, documentDirectory)).toMap
    implicit val semigroupFile: Semigroup[File] = (x: File, y: File) => {
      scribe.warn(s"douplicate image »$x« and »$y« discarding later")
      y
    }
    val blockImages = preprocessed.docCtx.flatMap {
      case (doc, ctx) =>
        val dedup = ctx.convertBlocks.map(b => b.attributes -> b).toMap.valuesIterator.toList
        val blockmaps: Map[Attributes, Map[ImageTarget, File]] =
          Monoid.combineAll(converters.map { case (target, ic) =>
            val blockFiles = dedup.map(ic.convertBlock(doc.file, _))
            Map.from(dedup.map(_.attributes).zip(blockFiles).collect {
              case (attr, Some(f)) => attr -> Map(target -> f)
            })
          })

        val dedupMacros =
          ctx.imageMacros.map(_.attributes).toSet.groupBy((attr: Attributes) => attr.named.contains("converter"))

        val imageattr = Monoid.combineAll(dedupMacros.getOrElse(false, Set.empty).iterator.map {
          (attributes: Attributes) =>
            val file = project.resolve(doc.file.parent, attributes.target)
            Map(attributes -> Map.from(targets.flatMap { t =>
              if (t.requiresConversion(attributes.target) && file.isDefined) {
                Some((t -> converters(t).applyConversion(file.get)))
              } else None
            }))
        })

        val imageconvert = Monoid.combineAll(dedupMacros.getOrElse(true, Set.empty).iterator.map {
          (attributes: Attributes) =>
            val file = project.resolve(doc.file.parent, attributes.target)
            scribe.info(s"converting $file")
            scribe.info(s"converting $attributes")
            Map(attributes -> Map.from(targets.flatMap { t =>
              if (file.isDefined) {
                converters(t).convertString(
                  attributes.named("converter"),
                  attributes,
                  Prov(),
                  file.get.contentAsString,
                  file.get
                ).map(res => (t -> res))
              } else None
            }))
        })

        List(blockmaps, imageattr, imageconvert)

    }

    ImageSubstitutions(Monoid.combineAll(blockImages))
  }
}

class ImageConverter(
    project: Project,
    imageTarget: ImageTarget,
    documentDirectory: DocumentDirectory,
) {

  def preferredFormat: String         = imageTarget.preferredFormat
  def unsupportedFormat: List[String] = imageTarget.unsupportedFormat

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
        Some(graphviz(templatedContent, project.cacheDir, preferredFormat))
      case rex"mermaid" =>
        Some(mermaid(templatedContent, project.cacheDir, preferredFormat))
      case other =>
        scribe.warn(s"unknown converter $other")
        None
    }
  }

  def graphviz(content: String, working: File, format: String): File = {
    val bytes  = content.getBytes(StandardCharsets.UTF_8)
    val hash   = Hashes.sha1hex(bytes)
    val dir    = working / hash
    val target = dir / (hash + s".$format")
    if (!target.exists) {
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

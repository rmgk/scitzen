package scitzen.extern

import better.files.{File, _}
import cats.instances.map.catsKernelStdMonoidForMap
import cats.kernel.{Monoid, Semigroup}
import scitzen.extern.ImageTarget.Undetermined
import scitzen.generic.{DocumentDirectory, PreprocessedResults, Project}
import scitzen.outputs.{Includes, SastToTextConverter}
import scitzen.parser.Parse
import scitzen.sast.{Attributes, Block, Fenced, Prov}

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

sealed class ImageTarget(val name: String, val preferredFormat: String, val unsupportedFormat: List[String]) {
  def requiresConversion(filename: String): Boolean = unsupportedFormat.exists(fmt => filename.endsWith(fmt))
}
object ImageTarget {
  object Undetermined extends ImageTarget("undetermined target", "", Nil)
  object Html         extends ImageTarget("html target", "svg", List("pdf", "tex"))
  object Tex          extends ImageTarget("tex target", "pdf", List("svg", "tex"))
  object Raster       extends ImageTarget("raster target", "png", List("svg", "pdf", "tex"))
}

case class ITargetPrediction(project: Project, cwd: File) {
  import scitzen.extern.ImageTarget._
  def predictMacro(attributes: Attributes): Attributes = {
    List(Html, Tex, Raster).foldLeft(attributes) { case (attr, target) =>
      if (target.requiresConversion(attr.target)) {
        val abs        = project.resolve(cwd, attr.target).get
        val filename   = s"${abs.nameWithoutExtension(false)}.${target.preferredFormat}"
        val rel        = project.root.relativize(abs)
        val prediction = project.cacheDir / "convertedImages" / rel.toString / filename
        attr.updated(s"${target.name}", project.relativizeToProject(prediction).toString)
      } else attr
    }
  }

  def predictBlock(attributes: Attributes): Attributes = {
    List(Html, Tex, Raster).foldLeft(attributes) { case (attr, target) =>
      val hash       = attributes.named("content hash")
      val prediction = project.cacheDir / hash / s"$hash.${target.preferredFormat}"
      attr.updated(s"${target.name}", project.relativizeToProject(prediction).toString)
    }
  }
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
      scribe.warn(s"duplicate image »$x« and »$y« discarding later")
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

        val imageattr = Monoid.combineAll(ctx.imageMacros.map(_.attributes).toSet.iterator.map {
          (attributes: Attributes) =>
            val file = project.resolve(doc.file.parent, attributes.target)
            Map(attributes -> Map.from(targets.flatMap { t =>
              if (t.requiresConversion(attributes.target) && file.isDefined) {
                converters(t).applyConversion(file.get, attributes).map(f => (t -> f))
              } else None
            }))
        })


        List(blockmaps, imageattr)

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

  def applyConversion(file: File, attributes: Attributes): Option[File] = {
    preferredFormat match {
      case "svg" | "png" if (file.extension.contains(".pdf")) => Some(pdfToCairo(file))
      case "pdf" | "png" if (file.extension.contains(".svg")) => Some(svgToCairo(file))
      case _ if (file.extension.contains(".tex")) =>
        val dir = (project.cacheDir / "convertedImages").path.resolve(project.root.relativize(file))
        val templated = applyTemplate(attributes, file.contentAsString, file.parent)
        convertTemplated("tex", templated, dir, file.nameWithoutExtension(includeAll = false))
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
    val relative = project.root.relativize(file)
    val targetfile = {
      if (project.cacheDir.isParentOf(file)) file.sibling(file.nameWithoutExtension(includeAll = false) + s".$preferredFormat")
      else File((project.cacheDir / "convertedImages")
           .path.resolve(relative)
           .resolve(file.nameWithoutExtension + s".$preferredFormat"))
    }
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
    val templated = applyTemplate( tlb.attributes, content, cwd)
    val hash =  tlb.attributes.named("content hash")
    convertTemplated(converter, templated, project.cacheDir / hash, hash)
  }

  def convertTemplated(
      converter: String,
      templatedContent: String,
      dir: File,
      name: String,
  ): Option[File] = {

    converter match {
      case "tex" =>
        val pdffile = texconvert(templatedContent, dir, name)
        if (preferredFormat == "svg" || preferredFormat == "png") Some(pdfToCairo(pdffile))
        else Some(pdffile)
      case gr @ "graphviz" =>
        Some(graphviz(templatedContent, dir, name, preferredFormat))
      case "mermaid" =>
        Some(mermaid(templatedContent, dir, name, preferredFormat))
      case other =>
        scribe.warn(s"unknown converter $other")
        None
    }
  }

  def applyTemplate(attributes: Attributes, content: String, cwd: File): String = {
    val templatedContent = attributes.named.get("template").flatMap(project.resolve(cwd, _)) match {
      case None               => content
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
    templatedContent
  }

  def graphviz(content: String, dir: File, name: String, format: String): File = {
    val bytes  = content.getBytes(StandardCharsets.UTF_8)
    val target = dir / (name + s".$format")
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

  def mermaid(content: String, dir: File, name: String, format: String): File = {
    val bytes         = content.getBytes(StandardCharsets.UTF_8)
    val target        = dir / (name + s".$format")
    val mermaidSource = dir / (name + ".mermaid")
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

  def texconvert(content: String, dir: File, name: String): File = {
    val target = dir / (name + ".pdf")
    if (target.exists) target
    else {
      val texbytes = content.getBytes(StandardCharsets.UTF_8)
      val dir      = target.parent
      dir.createDirectories()
      val texfile = dir / (name + ".tex")
      texfile.writeByteArray(texbytes)
      Latexmk.latexmk(dir, name, texfile)
    }
    target
  }

}

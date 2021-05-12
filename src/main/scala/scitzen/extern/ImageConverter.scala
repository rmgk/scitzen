package scitzen.extern

import better.files.{File, _}
import scitzen.generic.RegexContext.regexStringContext
import scitzen.generic.{DocumentDirectory, Project}
import scitzen.outputs.{Includes, SastToTextConverter}
import scitzen.parser.Parse
import scitzen.sast.{Attributes, Block, Fenced, Prov}

import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._

trait ConvertTask {
  def run(): Unit
}

class ConvertSchedulable[T](val data: T, val task: Option[ConvertTask]) {
  def map[U](f: T => U): ConvertSchedulable[U] = new ConvertSchedulable[U](f(data), task)
}

class ImageConverter(
    project: Project,
    val preferredFormat: String,
    unsupportedFormat: List[String] = Nil,
    documentDirectory: DocumentDirectory
) {

  def requiresConversion(filename: String): Boolean =
    unsupportedFormat.exists(fmt => filename.endsWith(fmt))

  def convertBlock(cwd: File, tlb: Block): ConvertSchedulable[Option[File]] = {
    val converter = tlb.attributes.named("converter")
    val content   = tlb.content.asInstanceOf[Fenced].content
    convertString(converter, tlb.attributes, tlb.prov, content, cwd) match {
      case None =>
        new ConvertSchedulable(None, None)
      case Some(res) => res.map(Some.apply)

    }
  }

  def applyConversion(file: File): ConvertSchedulable[File] = {
    preferredFormat match {
      case "svg" | "png" if (file.extension.contains(".pdf")) => {
        val (svgfile, tasko) = pdfToCairo(file)
        new ConvertSchedulable[File](svgfile, tasko)
      }
      case "pdf" | "png" if (file.extension.contains(".svg")) => {
        val (svgfile, tasko) = svgToCairo(file)
        new ConvertSchedulable[File](svgfile, tasko)
      }
    }

  }

  trait CommandFunction {
    def genCommand(source: File, target: File): List[String]
  }

  def pdfToCairo(file: File): (File, Option[ConvertTask]) = {
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

  private def convertExternal(file: File, command: CommandFunction): (File, Option[ConvertTask]) = {
    val relative = project.root.relativize(file.parent)
    val targetfile = File((project.cacheDir / "convertedImages")
      .path.resolve(relative)
      .resolve(file.nameWithoutExtension + s".$preferredFormat"))
    (
      targetfile,
      if (targetfile.exists) None
      else {
        Some(new ConvertTask {
          override def run(): Unit = {
            targetfile.parent.createDirectories()
            scribe.debug(s"converting $file to $targetfile")
            new ProcessBuilder(command.genCommand(file, targetfile).asJava)
              .inheritIO().start().waitFor()
          }
        })
      }
    )
  }

  def svgToCairo(file: File): (File, Option[ConvertTask]) = {
    convertExternal(
      file,
      (source, target) => {
        List("cairosvg", source.pathAsString, "-o", target.pathAsString)
      }
    )
  }

  def convertString(
      converter: String,
      attributes: Attributes,
      prov: Prov,
      content: String,
      cwd: File
  ): Option[ConvertSchedulable[File]] = {

    def applyConversion(data: (String, File, Option[ConvertTask])) = {
      val (_, _, convertTaskO) = data

      val (resfile, task2) =
        if (preferredFormat == "svg" || preferredFormat == "png")
          pdfToCairo(data._2)
        else (data._2, None)
      val combinedTask = (convertTaskO, task2) match {
        case (Some(l), Some(r)) => Some(new ConvertTask {
            override def run(): Unit = { l.run(); r.run() }
          })
        case (Some(r), None) => Some(r)
        case (None, Some(l)) => Some(l)
        case _               => None
      }

      new ConvertSchedulable(resfile, combinedTask)
    }

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
        Some(applyConversion(texconvert(templatedContent, project.cacheDir)))

      case gr @ rex"graphviz.*" =>
        Some(graphviz(templatedContent, project.cacheDir, gr.split("\\s+", 2).lift(1), preferredFormat))
      case rex"mermaid" =>
        Some(mermaid(templatedContent, project.cacheDir, preferredFormat))
      case other =>
        scribe.warn(s"unknown converter $other")
        None
    }
  }

  def graphviz(content: String, working: File, layout: Option[String], format: String): ConvertSchedulable[File] = {
    val bytes  = (s"//$layout\n" + content).getBytes(StandardCharsets.UTF_8)
    val hash   = Hashes.sha1hex(bytes)
    val dir    = working / hash
    val target = dir / (hash + s".$format")
    new ConvertSchedulable(
      target,
      if (target.exists) None
      else
        Some(new ConvertTask {
          override def run(): Unit = {
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
        })
    )
  }

  def mermaid(content: String, working: File, format: String): ConvertSchedulable[File] = {
    val bytes         = content.getBytes(StandardCharsets.UTF_8)
    val hash          = Hashes.sha1hex(bytes)
    val dir           = working / hash
    val target        = dir / (hash + s".$format")
    val mermaidSource = dir / (hash + ".mermaid")
    new ConvertSchedulable(
      target,
      if (target.exists) None
      else
        Some(new ConvertTask {
          override def run(): Unit = {

            val start = System.nanoTime()

            mermaidSource.createIfNotExists(createParents = true)
            mermaidSource.writeByteArray(bytes)

            new ProcessBuilder("mmdc", "--input", mermaidSource.pathAsString, "--output", target.pathAsString)
              .inheritIO().start().waitFor()
            scribe.info(s"mermaid compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
          }
        })
    )
  }

  def texconvert(content: String, working: File): (String, File, Option[ConvertTask]) = {
    val hash   = Hashes.sha1hex(content)
    val dir    = working / hash
    val target = dir / (hash + ".pdf")
    (
      hash,
      target,
      if (target.exists) None
      else {
        Some(new ConvertTask {
          override def run(): Unit = {
            val texbytes = content.getBytes(StandardCharsets.UTF_8)
            val dir      = target.parent
            dir.createDirectories()
            val texfile = dir / (hash + ".tex")
            texfile.writeByteArray(texbytes)
            Latexmk.latexmk(dir, hash, texfile)
          }
        })
      }
    )
  }

}

package scitzen.extern

import better.files.File
import scitzen.generic.RegexContext.regexStringContext
import scitzen.generic.{ConversionContext, Project}
import scitzen.outputs.SastToTextConverter
import scitzen.parser.Sast.{Block, Fenced, Macro}
import scitzen.parser.{Attribute, Attributes, MacroCommand, Parse, Prov, Sast}

import scala.jdk.CollectionConverters._

trait ConvertTask {
  def run(): Unit
}

class ConvertSchedulable[T](data: T, task: Option[ConvertTask]) {
  def map[U](f: T => U): ConvertSchedulable[U] = new ConvertSchedulable[U](f(data), task)
  def schedule(ctx: ConversionContext[_]): ConversionContext[T] = {
    task.fold(ctx.ret(data)) { task =>
      ctx.copy(data = data, tasks = task :: ctx.tasks)
    }
  }
}

class ImageConverter(project: Project, val preferredFormat: String, unsupportedFormat: List[String] = Nil) {

  def requiresConversion(filename: String): Boolean =
    unsupportedFormat.exists(fmt => filename.endsWith(fmt))

  def convertMacroTargetFile(cwd: File, mcro: Macro): ConvertSchedulable[Macro] = {
    val converter = mcro.attributes.named("converter")
    project.resolve(cwd, mcro.attributes.target) flatMap { file =>
      val content = file.contentAsString
      convertString(converter, mcro.attributes, content, cwd)
    } match {
      case None =>
        new ConvertSchedulable(
          mcro.copy(attributes =
            mcro.attributes.copy(raw = mcro.attributes.raw.filterNot(
              _.id == "converter"
            ))
          ),
          None
        )
      case Some(res) => res
    }
  }

  def convertBlock(cwd: File, tlb: Block): ConvertSchedulable[Sast] = {
    val converter = tlb.attributes.named("converter")
    val content   = tlb.content.asInstanceOf[Fenced].content
    convertString(converter, tlb.attributes, content, cwd) match {
      case None =>
        new ConvertSchedulable(tlb.copy(attributes = tlb.attributes.remove("converter")), None)
      case Some(res) => res.map(identity)

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
      content: String,
      cwd: File
  ): Option[ConvertSchedulable[Macro]] = {

    def makeImageMacro(file: File): Macro = {
      val relTarget = project.root.relativize(file)
      Macro(MacroCommand.Image, attributes.remove("converter").append(List(Attribute("", s"/$relTarget"))))
    }

    def applyConversion(data: (String, File, Option[ConvertTask])) = {
      val (hash, pdf, convertTaskO) = data

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

      val mcro = makeImageMacro(resfile)
      new ConvertSchedulable(mcro, combinedTask)
    }

    val templatedContent = attributes.named.get("template").flatMap(project.resolve(cwd, _)) match {
      case None => content
      case Some(templateFile) =>
        val tc   = templateFile.contentAsString
        val sast = Parse.documentUnwrap(tc, Prov(0, tc.length))
        SastToTextConverter(project.config.definitions ++ attributes.named + (
          "template content" -> content
        )).convert(sast).mkString("\n")
    }

    converter match {
      case "tex" =>
        Some(applyConversion(TexConverter.convert(templatedContent, project.cacheDir)))

      case gr @ rex"graphviz.*" =>
        Some(Graphviz.convert(content, project.cacheDir, gr.split("\\s+", 2).lift(1), preferredFormat)
          .map(img => makeImageMacro(img)))
      case gr @ rex"mermaid" =>
        Some(Mermaid.convert(content, project.cacheDir, preferredFormat)
          .map { img =>
            val m = makeImageMacro(img)
            m.copy(attributes = m.attributes.updated("style", "background-color: white"))
          })

      case other =>
        scribe.warn(s"unknown converter $other")
        None
    }
  }
}

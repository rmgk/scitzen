package scitzen.extern

import better.files.File
import scitzen.generic.RegexContext.regexStringContext
import scitzen.generic.Sast.{Fenced, SBlock, SMacro}
import scitzen.generic.{ConversionContext, Project, Sast}
import scitzen.parser.{Attribute, Attributes, Macro, MacroCommand}

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

  def convert(cwd: File, mcro: Macro): ConvertSchedulable[Macro] = {
    val converter = mcro.attributes.named("converter")
    project.resolve(cwd, mcro.attributes.target) flatMap { file =>
      val content = file.contentAsString
      doConversion(converter, mcro.attributes, content)
    } match {
      case None      =>
        new ConvertSchedulable(mcro.copy(attributes = mcro.attributes.copy(raw = mcro.attributes.raw.filterNot(
          _.id == "converter"))), None)
      case Some(res) => res
    }
  }


  def convert(tlb: SBlock): ConvertSchedulable[Sast] = {
    val converter = tlb.attributes.named("converter")
    val content   = tlb.content.asInstanceOf[Fenced].content
    doConversion(converter, tlb.attributes, content) match {
      case None      =>
        new ConvertSchedulable(tlb.copy(attributes = tlb.attributes.remove("converter")), None)
      case Some(res) => res.map(SMacro)

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
    convertExternal(file, (source, target) => {
      preferredFormat match {
        case "png" | "jpeg" | "tiff" =>
          List("pdftocairo", "-singlefile", s"-$preferredFormat",
               source.pathAsString,
               target.sibling(target.nameWithoutExtension).pathAsString)

        case "svg" =>
          List("pdftocairo", s"-$preferredFormat",
               source.pathAsString,
               target.pathAsString)
      }
    })
  }

  private def convertExternal(file: File, command: CommandFunction): (File, Option[ConvertTask]) = {
    val relative   = project.root.relativize(file.parent)
    val targetfile = File((project.cacheDir / "convertedImages")
                          .path.resolve(relative)
                          .resolve(file.nameWithoutExtension + s".$preferredFormat"))
    (targetfile,
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
    })
  }

  def svgToCairo(file: File): (File, Option[ConvertTask]) = {
    convertExternal(file, (source, target) => {
      List("cairosvg", source.pathAsString, "-o", target.pathAsString)
    })
  }

  //def pdfToSvg(in: File): File = {
  //  val out = in.sibling(in.name + ".svg")
  //  if (out.exists) return out
  //  Process(List("inkscape", in.pathAsString, "--export-plain-svg", out.pathAsString)).!
  //  out
  //}


  def doConversion(converter: String, attributes: Attributes, content: String): Option[ConvertSchedulable[Macro]] = {


    def makeImageMacro(file: File): Macro = {
      val relTarget = project.root.relativize(file)
      Macro(MacroCommand.Image,
            attributes.remove("converter").append(List(Attribute("", s"/$relTarget"))))
    }

    def applyConversion(data: (String, File, Option[ConvertTask])) = {
      val (hash, pdf, convertTaskO) = data

      val (resfile, task2) = if (preferredFormat == "svg" || preferredFormat == "png")
                               pdfToCairo(data._2)
                             else (data._2, None)
      val combinedTask     = (convertTaskO, task2) match {
        case (Some(l), Some(r)) => Some(new ConvertTask {
          override def run(): Unit = {l.run(); r.run()}
        })
        case (Some(r), None)    => Some(r)
        case (None, Some(l))    => Some(l)
        case _                  => None
      }

      val mcro = makeImageMacro(resfile)
      new ConvertSchedulable(mcro, combinedTask)
    }

    converter match {
      case "tex"  =>
        val header =
          attributes.named.get("header")
                    .flatMap(p => project.resolve(project.root, p))
                    .map(_.contentAsString).getOrElse("")
        Some(applyConversion(TexTikz.convert(header + content, project.cacheDir)))
      case "tikz" =>
        Some(applyConversion(TexTikz.convertTikz(content, project.cacheDir)))

      case gr @ rex"graphviz.*" =>
        Some(Graphviz.convert(content,
                              project.cacheDir,
                              gr.split("\\s+", 2).lift(1),
                              preferredFormat)
                     .map(img => makeImageMacro(img)))
      case gr @ rex"mermaid"    =>
        Some(Mermaid.convert(content,
                             project.cacheDir,
                             preferredFormat)
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

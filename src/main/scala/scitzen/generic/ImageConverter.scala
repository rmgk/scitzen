package scitzen.generic

import better.files.File
import scitzen.extern.{Graphviz, TexTikz}
import scitzen.generic.RegexContext.regexStringContext
import scitzen.generic.Sast.{Fenced, SBlock, SMacro}
import scitzen.parser.{Attribute, Attributes, Macro, MacroCommand}


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

class ImageConverter(project: Project, val formatHint: String) {

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

  def pdftosvg(file: File): ConvertSchedulable[File] = {
    val (svgfile, tasko) = pdftosvg_(file)
    new ConvertSchedulable[File](svgfile, tasko)
  }

  private def pdftosvg_(file: File): (File, Option[ConvertTask]) = {
    val relative   = project.root.relativize(file.parent)
    val targetfile = File((project.cacheDir / "svgs").path.resolve(relative).resolve(file.nameWithoutExtension + ".svg"))
    (targetfile,
    if (targetfile.exists) None
    else {
      Some(new ConvertTask {
        override def run(): Unit = {
          targetfile.parent.createDirectories()
          scribe.debug(s"converting $file to $targetfile")
          new ProcessBuilder("pdftocairo", "-svg", file.toString(), targetfile.toString()).inheritIO().start().waitFor()
        }
      })
    })

  }

  def doConversion(converter: String, attributes: Attributes, content: String): Option[ConvertSchedulable[Macro]] = {


    def makeImageMacro(file: File) = {
      val relTarget = project.root.relativize(file)
      Macro(MacroCommand.Image,
            attributes.remove("converter").append(List(Attribute("", s"/$relTarget"))))
    }

    def applyConversion(data: (String, File, Option[ConvertTask])) = {
      val (hash, pdf, convertTaskO) = data

      val (resfile, task2) = if (formatHint == "svg") pdftosvg_(data._2)
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
                              gr.split("\\s+", 2)(1),
                              formatHint)
                     .map(svg => makeImageMacro(svg)))
      case other                =>
        scribe.warn(s"unknown converter $other")
        None
    }
  }
}

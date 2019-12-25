package scitzen.generic

import better.files.File
import scitzen.extern.{Graphviz, TexTikz}
import scitzen.generic.RegexContext.regexStringContext
import scitzen.generic.Sast.{MacroBlock, RawBlock, TLBlock}
import scitzen.parser.{Attribute, Attributes, Macro, MacroCommand}

class ImageConverter(project: Project, formatHint: String) {

  def convert(cwd: File, mcro: Macro): MacroBlock = {
    val converter = mcro.attributes.named("converter")
    project.resolve(cwd, mcro.attributes.target) flatMap { file =>
      val content = file.contentAsString
      doConversion(converter, mcro.attributes, content)
    } match {
      case None      => MacroBlock(mcro.copy(attributes = mcro.attributes.copy(raw = mcro.attributes.raw.filterNot(
        _.id == "converter"))))
      case Some(res) => res
    }
  }


  def convert(tlb: TLBlock): Sast = {
    val converter = tlb.attr.named("converter")
    val content   = tlb.content.asInstanceOf[RawBlock].content
    doConversion(converter, tlb.attr, content) match {
      case None      =>
        tlb.copy(attr = tlb.attr.remove("converter"))
      case Some(res) => res

    }
  }

  def pdftosvg(file: File): File = {
    val relative   = project.root.relativize(file.parent)
    val targetfile = File((project.cacheDir / "svgs").path.resolve(relative).resolve(file.nameWithoutExtension + ".svg"))
    if (targetfile.exists) targetfile
    else {
      targetfile.parent.createDirectories()
      new ProcessBuilder("pdftocairo", "-svg", file.toString(), targetfile.toString()).inheritIO().start().waitFor()
      targetfile
    }

  }

  def doConversion(converter: String, attributes: Attributes, content: String): Option[MacroBlock] = {


    def makeImageMacro(file: File) = {
      val relTarget = project.root.relativize(file)
      Some(MacroBlock(Macro(MacroCommand.Image,
                            attributes.remove("converter").append(List(Attribute("",
                                                                                 s"/$relTarget"))))))
    }

    def applyConversion(pdf: File) = {
      val result =
        if (formatHint == "svg") pdftosvg(pdf)
        else pdf
      makeImageMacro(result)
    }

    converter match {
      case "tex"  =>
        val header      =
          attributes.named.get("header")
                    .flatMap(p => project.resolve(project.root, p))
                    .map(_.contentAsString).getOrElse("")
        val (hash, pdf) = TexTikz.convert(header + content, project.cacheDir)
        applyConversion(pdf)
      case "tikz" =>
        val (hash, pdf) = TexTikz.convertTikz(content, project.cacheDir)
        applyConversion(pdf)

      case gr @ rex"graphviz.*" =>
        val (hash, svg) = Graphviz.convert(content,
                                           project.cacheDir,
                                           gr.split("\\s+", 2)(1),
                                           formatHint)
        scribe.info(s"converting $hash to $svg")
        makeImageMacro(svg)
      case other                =>
        scribe.warn(s"unknown converter $other")
        None
    }
  }
}

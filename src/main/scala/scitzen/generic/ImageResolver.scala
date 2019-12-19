package scitzen.generic

import java.nio.file.Path

import better.files.File
import scitzen.extern.{Graphviz, TexTikz}
import scitzen.generic.RegexContext.regexStringContext
import scitzen.generic.Sast.{MacroBlock, RawBlock, TLBlock}
import scitzen.parser.{Attribute, Attributes, Macro, MacroCommand}

case class ImageResolver(project: Project, postOutputDir: File, files: List[File] = Nil) {

  def resolve(cwd: File, target: String): Option[(ImageResolver, File)] = {
    project.resolve(cwd, target).map { source =>
      (copy(files = source :: files), source)
    }
  }

  def relativize(source: File): Path = postOutputDir.relativize(source)

  def convert(cwd: File, mcro: Macro, formatHint: String): List[Sast] = {
    mcro.attributes.named.get("converter") match {
      case Some(converter) =>
        val res = project.resolve(cwd, mcro.attributes.target) match {
          case None       => Nil
          case Some(file) =>
            val content = file.contentAsString
            doConversion(converter, mcro.attributes, content, formatHint)
        }
        if (res.isEmpty) {
          List(MacroBlock(mcro.copy(attributes = mcro.attributes.copy(raw = mcro.attributes.raw.filterNot(
            _.id == "converter")))))
        }
        else res

      case None =>
        scribe.error(s"no converter for conversion?")
        Nil
    }
  }


  def convert(tlb: TLBlock, formatHint: String): List[Sast] = {
    tlb.attr.named.get("converter") match {
      case Some(converter) =>
        val content = tlb.content.asInstanceOf[RawBlock].content
        val res     = doConversion(converter, tlb.attr, content, formatHint)
        if (res.isEmpty) {
          List(tlb.copy(attr = tlb.attr.remove("converter")))
        }
        else res
      case None            =>
        scribe.error(s"no converter for conversion?")
        Nil
    }
  }

  def pdftosvg(file: File): File = {
    val targetfile = file.parent / (file.nameWithoutExtension + ".svg")
    if (targetfile.exists) targetfile
    else {
      new ProcessBuilder("pdftocairo", "-svg", file.toString(), targetfile.toString()).inheritIO().start().waitFor()
      targetfile
    }

  }

  def doConversion(converter: String, attributes: Attributes, content: String, formatHint: String) = {


    def makeImageMacro(file: File) = {
      val relTarget = project.root.relativize(file)
      List(MacroBlock(Macro(MacroCommand.Image,
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
        Nil
    }
  }
}

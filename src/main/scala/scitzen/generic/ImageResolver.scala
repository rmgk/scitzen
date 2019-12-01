package scitzen.generic

import java.nio.file.Path

import better.files.File
import scitzen.extern.{Graphviz, TexTikz}
import scitzen.generic.RegexContext.regexStringContext
import scitzen.generic.Sast.{MacroBlock, RawBlock, TLBlock}
import scitzen.parser.{Attribute, Macro, MacroCommand}

case class ImageResolver(project: Project, postOutputDir: File, files: List[File] = Nil) {

  def resolve(cwd: File, target: String): Option[(ImageResolver, File)] = {
    project.resolve(cwd, target).map { source =>
      (copy(files = source :: files), source)
    }
  }

  def relativize(source: File): Path = postOutputDir.relativize(source)


  def convert(tlb: TLBlock, formatHint: String): List[Sast] = {

    def makeImageMacro(file: File) = {
      val relTarget = project.root.relativize(file)
      List(MacroBlock(Macro(MacroCommand.Image,
                            tlb.attr.append(List(Attribute("", s"/$relTarget"))))))
    }

    tlb.attr.named.get("converter") match {
      case Some("tikz")               =>
        val (hash, pdf) = TexTikz.convert(tlb.content.asInstanceOf[RawBlock].content, project.cacheDir)
        scribe.info(s"converting $hash to $pdf")
        makeImageMacro(pdf)
      case Some(gr @ rex"graphviz.*") =>
        val (hash, svg) = Graphviz.convert(tlb.content.asInstanceOf[RawBlock].content,
                                           project.cacheDir,
                                           gr.split("\\s+", 2)(1),
                                           formatHint)
        scribe.info(s"converting $hash to $svg")
        makeImageMacro(svg)
      case other                      =>
        scribe.warn(s"unknown converter $other")
        List(tlb.copy(attr = tlb.attr.copy(raw = tlb.attr.raw.filterNot(_.id == "converter"))))
    }
  }

}

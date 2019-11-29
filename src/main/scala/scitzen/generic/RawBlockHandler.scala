package scitzen.generic

import better.files.File
import scitzen.extern.{Graphviz, TexTikz}
import scitzen.generic.Sast.{MacroBlock, RawBlock, TLBlock}
import scitzen.parser.{Attribute, Macro, MacroCommand}

import scitzen.generic.RegexContext.regexStringContext


class RawBlockHandler(cachedir: File) {

  def convert(tlb: TLBlock, formatHint: String): List[Sast] = {

    def makeImageMacro(file: File) =
      List(MacroBlock(Macro(MacroCommand.Image,
                            tlb.attr.append(List(Attribute("", file.toString()))))))

    tlb.attr.named.get("converter") match {
      case Some("tikz")               =>
        val (hash, pdf) = TexTikz.convert(tlb.content.asInstanceOf[RawBlock].content, cachedir)
        scribe.info(s"converting $hash to $pdf")
        makeImageMacro(pdf)
      case Some(gr @ rex"graphviz.*") =>
        val (hash, svg) = Graphviz.convert(tlb.content.asInstanceOf[RawBlock].content,
                                           cachedir,
                                           gr.split("\\s+", 2)(1),
                                           formatHint)
        scribe.info(s"converting $hash to $svg")
        makeImageMacro(svg)
      case other                      =>
        scribe.warn(s"unknown converter $tlb")
        List(tlb)
    }
  }

}

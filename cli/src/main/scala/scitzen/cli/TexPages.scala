package scitzen.cli

import scitzen.converter.SastToTexConverter
import scitzen.semantics.Sast
import scitzen.semantics.SastAnalyzes.AnalyzeResult

object TexPages {


  def acmHeader(): String = {
"""% !TEX jobname = report
% !TEX output_directory = output
\documentclass[sigconf,screen=true,authorversion=false]{acmart}
\settopmatter{printfolios=true}
""" + // funny story, but just \ u is always some compile error
"\\usepackage[utf8x]{inputenc}" + """
\setcopyright{rightsretained}
"""
  }

  def wrap(analyzed: AnalyzeResult, sast: Sast, texType: String, bibliography: Option[String]) = {
    val content = new SastToTexConverter(analyzed).sastToTex(sast)
    texType match {
      case "acmconf" =>
        (List(
          acmHeader(),
          s"\\begin{document}"
          ) ++ content ++
         List(
           s"\\bibliographystyle{ACM-Reference-Format}",
           bibliography.fold("")(bib => s"\\bibliography{$bib}"),
           s"\\end{document}"
           )).mkString("\n")
    }

  }

}

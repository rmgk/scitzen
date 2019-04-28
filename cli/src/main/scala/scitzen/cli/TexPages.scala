package scitzen.cli

import scitzen.converter.SastToTexConverter
import scitzen.semantics.Sast
import scitzen.semantics.SastAnalyzes.AnalyzeResult

object TexPages {


  def header(): String = {
"""
\documentclass[sigconf,screen=true,authorversion=false]{acmart}
\settopmatter{printfolios=true}
""" + // funny story, but just \ u is always some compile error
"\\usepackage[utf8x]{inputenc}" + """
\input{src/imports}
"""
  }

  def wrap(analyzed: AnalyzeResult, sast: Sast) = {
    (List(
      header(),
      s"\\begin{document}"
      ) ++ new SastToTexConverter(analyzed).sastToTex(sast) ++
    List(
      s"\\bibliographystyle{ACM-Reference-Format}",
      s"\\bibliography{src/bibliography}",
      s"\\end{document}"
      )).mkString("\n")
  }

}

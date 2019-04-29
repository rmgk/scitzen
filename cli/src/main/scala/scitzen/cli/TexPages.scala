package scitzen.cli

import scitzen.parser.{Macro, Parse}
import scitzen.semantics.SastAnalyzes.AnalyzeResult

object TexPages {


  def acmHeader: String = {
"""% !TEX jobname = report
% !TEX output_directory = output
\documentclass[sigconf,screen=true,authorversion=false]{acmart}
\settopmatter{printfolios=true}
\setcopyright{rightsretained}
"""
  }

  def memoirHeader: String = {
"""% !TEX jobname = report
% !TEX output_directory = output
\documentclass[a4paper, oneside]{memoir}
\clubpenalty=10000
\widowpenalty=10000
"""
  }
  def memoirPackages: List[String] = {
    List("[utf8x]{inputenc}", "{graphicx}", "{url}", "{verbatim}")
  }

  def wrap(content: Seq[String], analyzed: AnalyzeResult, bibliography: Option[String]): String = {
    val authors = analyzed.named.get("authors").toList.flatMap {aut =>
      Parse.paragraph(aut).right.get.collect{
        case Macro("author", attributes) => (attributes.head.value, attributes.last.value)
      }
    }
    val authorstrings = authors.map{ case (name, inst) =>
      s"""\\author{$name}
\\affiliation{\\institution{$inst}}"""
    }
    (analyzed.named("layout").trim.toLowerCase match {
      case "acmconf" =>
        List(
          acmHeader,
          s"\\begin{document}"
          ) ++ authorstrings ++ content ++
           bibliography.fold(List.empty[String]){bib =>
             List(s"\\bibliographystyle{ACM-Reference-Format}",
             s"\\bibliography{$bib}")} :+
           s"\\end{document}"
      case "memoir" =>
        (memoirHeader +: memoirPackages.map(p => s"\\usepackage$p") :+ s"\\begin{document}") ++
        content :+ s"\\end{document}"

    }).mkString("\n")

  }

}

package scitzen.cli

import scitzen.parser.{Macro, Parse}
import scitzen.semantics.SastAnalyzes.AnalyzeResult

object TexPages {


  def acmHeader(): String = {
"""% !TEX jobname = report
% !TEX output_directory = output
\documentclass[sigconf,screen=true,authorversion=false]{acmart}
\settopmatter{printfolios=true}
""" + // funny story, but just \ u is always some compile error
"""
\setcopyright{rightsretained}
"""
  }

  def wrap(content: Seq[String], analyzed: AnalyzeResult, texType: String, bibliography: Option[String]) = {
    val authors = analyzed.named.get("authors").toList.flatMap {aut =>
      Parse.paragraph(aut).right.get.collect{
        case Macro("author", attributes) => (attributes.head.value, attributes.last.value)
      }
    }
    val authorstrings = authors.map{ case (name, inst) =>
      s"""\\author{$name}
\\affiliation{\\institution{$inst}}"""
    }
    texType match {
      case "acmconf" =>
        (List(
          acmHeader(),
          s"\\begin{document}"
          ) ++ authorstrings ++ content ++
         List(
           s"\\bibliographystyle{ACM-Reference-Format}",
           bibliography.fold("")(bib => s"\\bibliography{$bib}"),
           s"\\end{document}"
           )).mkString("\n")
    }

  }

}

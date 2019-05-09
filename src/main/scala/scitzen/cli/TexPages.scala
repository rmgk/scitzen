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
% set \clubpenalty, etc. to distinctive values for use
% in tracing page breaks. These values are chosen so that
% no single penalty will absolutely prohibit a page break, but
% certain combinations of two or more will.
\clubpenalt=9996
\widowpenalty=9999
\brokenpenalty=4991
% Reiterate the default value of \redisplaypenalty, for
% completeness.
% Set postdisplaypenalty to a fairly high value to discourage a
% page break between a display and a widow line at the end of a
% paragraph.
\predisplaypenalty=10000
\postdisplaypenalty=1549
% And then \displaywidowpenalty should be at least as high as
% \postdisplaypenalty, otherwise in a situation where two displays
% are separated by two lines, TeX will prefer to break between the
% two lines, rather than before the first line.
\displaywidowpenalty=1602

\setlength{\topskip}{1.6\topskip}
\checkandfixthelayout
\sloppybottom
"""
  }

  def thesisHeader: String = {
    """% !TEX jobname = report
    % !TEX output_directory = output
    \documentclass[a4paper, oneside]{memoir}
    """
  }

  def luatexPackages: List[String] = {
    List("\\usepackage{luatextra}", "\\defaultfontfeatures{Ligatures=TeX}")
  }

  def pdflatexPackages: List[String] = {
    List("\\usepackage[T1]{fontenc}", "\\usepackage[utf8x]{inputenc}")
  }

  def memoirPackages: List[String] = {
    List("{microtype}", "[german, english]{babel}", "{libertine}", "{graphicx}", "{url}", "{verbatim}")
  }

  def wrap(content: Seq[String], analyzed: AnalyzeResult, bibliography: Option[String]): String = {
    val authors = analyzed.named.get("authors").toList.flatMap {aut =>
      Parse.paragraph(aut).right.get.collect{
        case Macro("author", attributes) => (attributes.positional.head, attributes.positional.tail)
      }
    }
    val authorstrings = authors.map{ case (name, inst) =>
      s"""\\author{$name}
\\affiliation{\\institution{$inst}}"""
    }

    def importBibACM = {
      bibliography.fold(List.empty[String]) { bib =>
        List(s"\\bibliographystyle{ACM-Reference-Format}",
             s"\\bibliography{$bib}")
      }
    }
    def importBibNatbib = {
      bibliography.fold(List.empty[String]) { bib =>
        List(s"\\bibliographystyle{plain}",
             s"\\bibliography{$bib}")
      }
    }

    (analyzed.named("layout").trim.toLowerCase match {
      case "acmconf" =>
        List(
          acmHeader,
          s"\\begin{document}"
          ) ++ authorstrings ++ content ++ importBibACM :+
           s"\\end{document}"
      case "memoir" =>
        (memoirHeader +: (luatexPackages ++ memoirPackages.map(p => s"\\usepackage$p")) :+ s"\\begin{document}" :+ "\\sloppy") ++
        content :+ s"\\end{document}"
      case "thesis" =>
        (thesisHeader +: (luatexPackages ++ memoirPackages.map(p => s"\\usepackage$p")) :+ s"\\begin{document}") ++
        content ++ importBibNatbib :+ s"\\end{document}"

    }).mkString("\n")

  }

}

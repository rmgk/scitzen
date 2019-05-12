package scitzen.cli

import scitzen.parser.{Macro, Parse}

object TexPages {


  def acmHeader: String = {
"""% !TEX jobname = report
% !TEX output_directory = output
\documentclass[sigconf,screen=true,authorversion=false]{acmart}
\settopmatter{printfolios=true}
\setcopyright{rightsretained}
"""
  }

  def sloppyStuff: String = """
% set \clubpenalty, etc. to distinctive values for use
% in tracing page breaks. These values are chosen so that
% no single penalty will absolutely prohibit a page break, but
% certain combinations of two or more will.
\clubpenalty=9996
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

  def memoirHeader: String = {
"""\documentclass[a4paper, oneside]{memoir}"""
  }

  def usePackages(list: String*) : List[String] = list.map(p => s"\\usepackage$p").toList

  def luatexPackages: List[String] = {
    usePackages("{luatextra}", "[ngerman, english]{babel}") :+
    "\\defaultfontfeatures{Ligatures=TeX}"
  }

  def xelatexPackages: List[String] = {
    usePackages("{polyglossia}")++
    List("\\setmainlanguage{english}", "\\setotherlanguage{german}")
  }

  def pdflatexPackages: List[String] = {
    usePackages("[T1]{fontenc}", "[utf8x]{inputenc}", "[ngerman, english]{babel}")
  }

  def memoirPackages: List[String] = {
    usePackages("{microtype}", "{graphicx}", "[colorlinks]{hyperref}", "{verbatim}") ++
    List("\\nouppercaseheads")
  }

  def wrap(content: Seq[String], authorsStr: String, layout: String, bibliography: Option[String]): String = {
    val authors = {
      Parse.paragraph(authorsStr).right.get.collect{
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

    (layout match {
      case "acmconf" =>
        List(
          acmHeader,
          s"\\begin{document}"
          ) ++ authorstrings ++ content ++ importBibACM :+
           s"\\end{document}"
      case _ =>
        (memoirHeader +: sloppyStuff +: (xelatexPackages ++ memoirPackages) :+ s"\\begin{document}" :+ "\\sloppy") ++
        content ++ importBibNatbib :+ s"\\end{document}"

    }).mkString("\n")

  }

}

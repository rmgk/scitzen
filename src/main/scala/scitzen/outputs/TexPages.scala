package scitzen.outputs

import cats.data.Chain
import scitzen.parser.MacroCommand.Other
import scitzen.parser.{Macro, Parse, Prov}

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

  def lstlistings: String = """
% colors and definition for scalastyle
\u005Cusepackage{listings}
\u005Cusepackage{xcolor}
\definecolor{dkgreen}{rgb}{0,0.6,0}
\definecolor{gray}{rgb}{0.5,0.5,0.5}
\definecolor{mauve}{rgb}{0.58,0,0.82}

\lstdefinestyle{scitzenCodestyle}{
  morestring=[b]" ,
  keywords={},
  comment=[l]{//},
  otherkeywords={},
  morekeywords=[2]{},
  frame=none,
  aboveskip=3mm,
  belowskip=3mm,
  showstringspaces=false,
  %columns=flexible,
  basicstyle={\small\ttfamily},
  numbers=left,
  xleftmargin=1.9em,
  numberstyle=\tiny\color{gray},
  keywordstyle=\bfseries,
  keywordstyle=[2]\bfseries,
  commentstyle=\color{gray},
  breaklines=true,
  breakatwhitespace=true,
  tabsize=2,
  escapechar=§,
  backgroundcolor=\color{white},
  numberblanklines=true,
  firstnumber=last
}
\lstset{style=scitzenCodestyle}"""

  val memoirHeader: String = {
"""\documentclass[a4paper, oneside]{memoir}"""
  }

  def usePackages(list: String*) : List[String] = list.map(p => s"\\usepackage$p").toList

  val luatexPackages: List[String] = {
    usePackages("{luatextra}", "[ngerman, english]{babel}") :+
    "\\defaultfontfeatures{Ligatures=TeX}"
  }

  val xelatexPackages: List[String] = {
    usePackages("{polyglossia}")++
    List("\\setmainlanguage{english}", "\\setotherlanguage{german}")
  }

  // so, XeTeX does support unicode, but the default font (Computer Modern)
  // does not contain that many symbols.
  // using fontspec should load Latin Modern, which should contain more symbols
  // but seemingly still not enough
  val xelatexFont: List[String] = {
    usePackages("{fontspec}") :+
    "\\defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}" :+
    "\\setmainfont{Linux Libertine O}" :+
    "\\setsansfont{Linux Biolinum O}" :+
    "\\setmonofont{Linux Libertine Mono O}"
  }

  val pdflatexPackages: List[String] = {
    usePackages("[T1]{fontenc}", "[utf8x]{inputenc}", "[ngerman, english]{babel}")
  }

  val memoirPackages: List[String] = {
    List("\\PassOptionsToPackage{hyphens}{url}") ++
    usePackages("{microtype}", "{graphicx}", "[colorlinks]{hyperref}") ++
    List(lstlistings, "\\nouppercaseheads")
  }

  def wrap(content: Chain[String], authorsStr: String, layout: String, bibliography: Option[String]): String = {
    val authors = Chain.fromSeq{
      Parse.paragraph(authorsStr, Prov()).toTry.get.collect{
        case Macro(Other("author"), attributes) => (attributes.positional.head, attributes.positional.tail)
      }
    }
    val authorstrings = authors.map{ case (name, inst) =>
      s"""\\author{$name}
\\affiliation{\\institution{$inst}}"""
    }

    def importBibACM = Chain.fromSeq {
      bibliography.fold(List.empty[String]) { bib =>
        List(s"\\bibliographystyle{ACM-Reference-Format}",
             s"\\bibliography{$bib}")
      }
    }
    def importBibNatbib = Chain.fromSeq {
      bibliography.fold(List.empty[String]) { bib =>
        List(s"\\bibliographystyle{plain}",
             s"\\bibliography{$bib}")
      }
    }

    (layout match {
      case "acmconf" =>
        Chain(
          acmHeader,
          s"\\begin{document}"
          ) ++ authorstrings ++ content ++ importBibACM :+
           s"\\end{document}"
      case _ =>
        Chain.fromSeq(memoirHeader /*+: sloppyStuff*/ +:
                      (xelatexPackages ++ xelatexFont ++ memoirPackages) :+
         s"\\begin{document}" /*:+ "\\sloppy"*/) ++
        content ++ importBibNatbib :+ s"\\end{document}"

    }).iterator.mkString("\n")

  }

}

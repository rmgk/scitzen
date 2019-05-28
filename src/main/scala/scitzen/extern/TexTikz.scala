package scitzen.extern

import java.nio.charset.StandardCharsets

import better.files.File

import scala.sys.process.Process


object TexTikz {

  val header = """
    |\documentclass{standalone}
    |\u005Cusepackage{tikz}
    |\u005Cusetikzlibrary{shapes,backgrounds,calc,positioning}
    |
    |\u005Cusepackage{fontspec}
    |\defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}
    |\setmainfont{Liberation Serif}
    |
    |\begin{document}
    |""".stripMargin
  val footer = """
    |\end{document}
    |""".stripMargin

  def latexmk(outputdir: File, jobname: String, sourceFile: File): File = {
    val start = System.nanoTime()
    Process(List("latexmk",
                 "-cd",
                 "-f",
                 "-xelatex",
                 "-interaction=nonstopmode",
                 //"-synctex=1",
                 "--output-directory=" + outputdir,
                 "--jobname=" + jobname,
                 sourceFile.pathAsString)).!
    scribe.info(s"tex compilation finished in ${(System.nanoTime() - start)/1000000}ms")
    outputdir / (jobname + ".pdf")
  }

  def convert(content: String, working: File): (String, File) = {
    val hash = Hashes.sha1hex(content)
    val dir = working / hash
    val target = dir / (hash + ".pdf")
    if (target.exists) return hash -> target

    val texstring = header + content + footer
    val texbytes = texstring.getBytes(StandardCharsets.UTF_8)


    dir.createDirectories()
    val texfile = dir / (hash + ".tex")
    texfile.writeByteArray(texbytes)
    latexmk(dir, hash, texfile)
    hash -> target
  }


}

package scitzen.extern

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import scala.sys.process.Process
import better.files.File

object Hashes {
  private val sha1digester: MessageDigest = MessageDigest.getInstance("SHA-1")
  def sha1(b: Array[Byte]): Array[Byte] = sha1digester.clone().asInstanceOf[MessageDigest].digest(b)
  def sha1hex(b: Array[Byte]): String = sha1(b).map { h => f"$h%02x" }.mkString
}


object Tex {

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

  def convert(content: String, working: File): File = {
    val texstring = header + content + footer
    val texbytes = texstring.getBytes(StandardCharsets.UTF_8)
    val hash = Hashes.sha1hex(texbytes)
    val dir = working / hash
    val target = dir / (hash + ".pdf")
    if (target.exists) return target


    dir.createDirectories()
    val texfile = dir / (hash + ".tex")
    texfile.writeByteArray(texbytes)
    latexmk(dir, hash, texfile)
    target
  }

  def pdfToSvg(in: File): File = {
    val out = in.sibling(in.name + ".svg")
    if (out.exists) return out
    Process(List("inkscape", in.pathAsString, "--export-plain-svg", out.pathAsString)).!
    out
  }

}

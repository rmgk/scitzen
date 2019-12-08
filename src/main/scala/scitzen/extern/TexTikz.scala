package scitzen.extern

import java.nio.charset.StandardCharsets

import better.files.File
import scitzen.outputs.TexPages


object TexTikz {

  val header: String = """
    |\documentclass{standalone}
    |\u005Cusepackage{tikz}
    |\u005Cusetikzlibrary{shapes,backgrounds,calc,positioning}
    """.stripMargin + TexPages.xelatexFont.mkString("\n") + """
    |
    |\begin{document}
    |""".stripMargin
  val footer: String = """
    |\end{document}
    |""".stripMargin

  def latexmk(outputdir: File, jobname: String, sourceFile: File): File = {
    val start = System.nanoTime()
    outputdir.createDirectories()
    new ProcessBuilder("latexmk",
                       "-cd",
                       "-f",
                       "-xelatex",
                       "-interaction=nonstopmode",
                       //"-synctex=1",
                       "--output-directory=" + outputdir,
                       "--jobname=" + jobname,
                       sourceFile.pathAsString).inheritIO()
                                               .redirectOutput((outputdir / "latexmk.out").toJava)
                                               .start().waitFor()
    scribe.info(s"tex compilation finished in ${(System.nanoTime() - start)/1000000}ms")
    outputdir / (jobname + ".pdf")
  }

  def pdfcrop(input: File, output: File): Unit = {
    new ProcessBuilder("pdfcrop", input.toString(), output.toString()).inheritIO().start().waitFor()
  }

  def convert(content: String, working: File): (String, File) = {
    val hash = Hashes.sha1hex(content)
    val dir = working / hash
    val target = dir / (hash + ".pdf")
    if (target.exists) return hash -> target
    scribe.info(s"converting inline file to $target")
    val texbytes = content.getBytes(StandardCharsets.UTF_8)
    dir.createDirectories()
    val texfile = dir / (hash + ".tex")
    texfile.writeByteArray(texbytes)
    latexmk(dir, hash, texfile)
    //pdfcrop(res, target)
    hash -> target

  }

  def convertTikz(content: String, working: File): (String, File) = {
    convert(header + content + footer, working)
  }


}

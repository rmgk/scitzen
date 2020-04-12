package scitzen.extern

import java.nio.charset.StandardCharsets

import better.files.File
import scitzen.outputs.TexPages


object TexTikz {
  def wrap(content: String) = header + content + footer


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
    scribe.info(s"tex compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    outputdir / (jobname + ".pdf")
  }

  def pdfcrop(input: File, output: File): Unit = {
    new ProcessBuilder("pdfcrop", input.toString(), output.toString()).inheritIO().start().waitFor()
  }


  def convert(content: String, working: File): (String, File, Option[ConvertTask]) = {
    val hash   = Hashes.sha1hex(content)
    val dir    = working / hash
    val target = dir / (hash + ".pdf")
    (hash, target, if (target.exists) None else {
      Some(new ConvertTask {
        override def run(): Unit = {
          val texbytes = content.getBytes(StandardCharsets.UTF_8)
          val dir      = target.parent
          dir.createDirectories()
          val texfile = dir / (hash + ".tex")
          texfile.writeByteArray(texbytes)
          latexmk(dir, hash, texfile)
        }
      })
    })
  }

  def convertTikz(content: String, working: File): (String, File, Option[ConvertTask]) = {
    convert(header + content + footer, working)
  }


}

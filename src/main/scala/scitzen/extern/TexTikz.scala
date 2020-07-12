package scitzen.extern

import java.nio.charset.StandardCharsets

import better.files.File
import scitzen.outputs.TexPages


object TexTikz {


  val header: String = """
    |\documentclass{standalone}
    |\usepackage{tikz}
    |\usepackage{booktabs}
    |\usetikzlibrary{shapes,backgrounds,calc,positioning}
    """.stripMargin + TexPages.xelatexFont.mkString("\n") + """
    |
    |\begin{document}
    |""".stripMargin
  val footer: String = """
    |\end{document}
    |""".stripMargin

  def latexmk(outputdir: File, jobname: String, sourceFile: File): Option[File] = {
    val start = System.nanoTime()
    scribe.info(s"compiling $sourceFile")
    outputdir.createDirectories()
    val errorFile = (outputdir/"latexmk.err")
    val returnCode = new ProcessBuilder("latexmk",
                       "-cd",
                       "-halt-on-error",
                       "-xelatex",
                       "-interaction=nonstopmode",
                       //"-synctex=1",
                       "--output-directory=" + outputdir,
                       "--jobname=" + jobname,
                       sourceFile.pathAsString)
                                               .redirectOutput((outputdir / "latexmk.out").toJava)
                                               .redirectError(errorFile.toJava)
                                               .start().waitFor()
    if (returnCode == 0) {
      scribe.info(s"tex compilation of »$sourceFile« finished in ${(System.nanoTime() - start) / 1000000}ms")
      Some(outputdir / (jobname + ".pdf"))
    }
    else {
      scribe.error(s"error tex compiling »$sourceFile« see »$errorFile«")
      None
    }
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

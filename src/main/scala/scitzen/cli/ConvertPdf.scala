package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}

import better.files.File
import scitzen.extern.ImageConverter
import scitzen.extern.TexTikz.latexmk
import scitzen.generic.{ConversionContext, Project}
import scitzen.outputs.{SastToTexConverter, TexPages}

object ConvertPdf {
  implicit val charset: Charset = StandardCharsets.UTF_8

  def convertToPdf(project: Project): Unit = {

    val preprocessed = Common.preprocessDocuments(project, new ImageConverter(project, "pdf", List("svg")))

    preprocessed.articles.foreach { article =>
      val resultContext = new SastToTexConverter(
        project,
        article.sourceDoc.file.parent,
        article.sourceDoc.reporter,
        preprocessed.directory
      ).convert(
        article.content
      )(ConversionContext((), labelledThings = preprocessed.labels))

      val content = resultContext.data

      resultContext.execTasks()

      val articlename = Format.canonicalName(article)

      val outputdir = project.outputdir / "pdfs"
      outputdir.createDirectories()

      val targetfile = outputdir / s"$articlename.pdf"

      def fileFromParam(param: String): Option[File] = {
          article.named.get(param).map(s => article.sourceDoc.file.parent / s.trim)
      }

      val bibliography = fileFromParam("bibliography").map(_.pathAsString)
      scribe.debug(s"bib is $bibliography")
      val authors = article.named.get("authors").getOrElse("")

      val macros = fileFromParam("texMathMacroFile").map(_.contentAsString).getOrElse("")

      val jobname     = targetfile.nameWithoutExtension(includeAll = false)
      val temptexfile = project.cacheDir / (jobname + ".tex")
      val temptexdir  = project.cacheDir / s"$articlename.out"
      temptexfile.write(TexPages.wrap(content, authors, "memoir", bibliography, macros))
      latexmk(temptexdir, jobname, temptexfile).foreach(_.copyTo(targetfile, overwrite = true))
    }
  }

}

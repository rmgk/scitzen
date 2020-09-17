package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}

import better.files.File
import scitzen.extern.ImageConverter
import scitzen.extern.TexConverter.latexmk
import scitzen.generic.{ConversionContext, DocumentDirectory, Project}
import scitzen.outputs.{SastToTexConverter, SastToTextConverter}
import scitzen.parser.Parse
import scitzen.sast.Prov

object ConvertPdf {
  implicit val charset: Charset = StandardCharsets.UTF_8

  def convertToPdf(project: Project, documentDirectory: DocumentDirectory): Unit = {

    val preprocessed = Common.preprocessDocuments(
      project,
      new ImageConverter(project, "pdf", List("svg")),
      documentDirectory
    )

    preprocessed.articles.foreach { article =>
      val converter = new SastToTexConverter(
        project,
        article.sourceDoc.file,
        article.sourceDoc.reporter,
        preprocessed.directory
      )

      val resultContext =
        converter.convert(article.content)(ConversionContext((), labelledThings = preprocessed.labels))

      val headerres = converter.articleHeader(article, resultContext)

      val content = headerres.data ++ resultContext.data

      headerres.execTasks()

      val articlename = Format.canonicalName(article)

      val outputdir = project.outputdir / "pdfs"
      outputdir.createDirectories()

      val targetfile = outputdir / s"$articlename.pdf"

      def fileFromParam(param: String): Option[File] = {
        article.named.get(param).map(s => article.sourceDoc.file.parent / s.trim)
      }

      val bibliography = fileFromParam("bibliography").map(_.pathAsString)
      scribe.debug(s"bib is $bibliography")

      val jobname     = targetfile.nameWithoutExtension(includeAll = false)
      val temptexfile = project.cacheDir / (jobname + ".tex")
      val temptexdir  = project.cacheDir / s"$articlename.outdir"

      val template        = article.named.get("texTemplate").orElse(project.config.texTemplate).get
      val templateContent = project.resolve(project.root, template).get.contentAsString
      val templateSast    = Parse.documentUnwrap(templateContent, Prov(0, templateContent.length))
      val templateSettings: Map[String, String] =
        project.config.definitions ++ article.header.attributes.raw.map(a => (a.id -> a.value)) ++ List(
          Some("template content"              -> content.iterator.mkString("\n")),
          bibliography.map("bibliography path" -> _)
        ).flatten ++ resultContext.features.toList.map(s => s"feature $s" -> "")

      val documentString = SastToTextConverter(templateSettings).convert(templateSast).mkString("\n")
      temptexfile.write(documentString)
      latexmk(temptexdir, jobname, temptexfile).foreach(_.copyTo(targetfile, overwrite = true))
    }
  }

}

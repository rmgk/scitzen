package scitzen.cli

import better.files.File
import better.files.File.CopyOptions
import scitzen.contexts.ConversionContext
import scitzen.extern.Latexmk
import scitzen.generic.{DocumentDirectory, PreprocessedResults, Project}
import scitzen.outputs.SastToTexConverter

import java.nio.charset.{Charset, StandardCharsets}

object ConvertPdf {
  implicit val charset: Charset = StandardCharsets.UTF_8

  def convertToPdf(project: Project, documentDirectory: DocumentDirectory): Unit = {

    val preprocessed = new PreprocessedResults(
      project,
      documentDirectory.documents
    )

    // val converter = new ImageConverter(project, "pdf", List("svg"), documentDirectory)


    import scala.jdk.CollectionConverters._
    preprocessed.articles
      .filter(_.header.attributes.named.contains("texTemplate"))
      .asJava.parallelStream().forEach { article =>
        val converter = new SastToTexConverter(
          project,
          article.sourceDoc.file,
          article.sourceDoc.reporter,
          preprocessed.directory,
          preprocessed.labels
        )

        val resultContext =
          converter.convert(article.content)(ConversionContext(()))

        val headerres = converter.articleHeader(article, resultContext)

        val content = headerres.data ++ resultContext.data

        val articlename = Format.canonicalName(article)

        val outputdir = project.outputdir / "pdfs"
        outputdir.createDirectories()

        val targetfile = outputdir / s"$articlename.pdf"

        def fileFromParam(param: String): Option[File] = {
          article.named.get(param).map(s => article.sourceDoc.file.parent / s.trim)
        }

        val jobname     = targetfile.nameWithoutExtension(includeAll = false)
        val temptexdir  = project.cacheDir / s"$articlename.outdir"
        temptexdir.createDirectories()
        val temptexfile = temptexdir / (jobname + ".tex")

        val bibFile      = fileFromParam("bibliography")
        val bibliography = bibFile.map(_ => "bibliography.bib")
        bibFile.foreach(_.copyTo(temptexdir / "bibliography.bib")(copyOptions = CopyOptions(overwrite = true)))

        val templateSettings =
          project.config.definitions ++ article.header.attributes.raw.map(a => (a.id -> a.value)) ++ List(
            Some("template content"              -> content.iterator.mkString("\n")),
            bibliography.map("bibliography path" -> _)
          ).flatten ++ resultContext.features.toList.map(s => s"feature $s" -> "")

        val documentString: String =
          ConvertTemplate.fillTemplate(
            project,
            preprocessed.directory,
            article.named.get("texTemplate").orElse(project.config.texTemplate).get,
            templateSettings
          )
        temptexfile.write(documentString)
        Latexmk.latexmk(temptexdir, jobname, temptexfile).foreach(_.copyTo(targetfile, overwrite = true))
      }
  }
}

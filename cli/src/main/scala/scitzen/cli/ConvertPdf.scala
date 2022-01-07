package scitzen.cli

import better.files.File
import better.files.File.CopyOptions
import scitzen.contexts.ConversionContext
import scitzen.extern.{Hashes, Latexmk}
import scitzen.generic.{PreprocessedResults, Project}
import scitzen.outputs.SastToTexConverter

import java.nio.charset.{Charset, StandardCharsets}
import scala.jdk.CollectionConverters._

object ConvertPdf:
  implicit val charset: Charset = StandardCharsets.UTF_8

  def convertToPdf(
      project: Project,
      preprocessed: PreprocessedResults,
  ): Unit =
    preprocessed.articles
      .filter(_.header.attributes.named.contains("texTemplate"))
      .asJava.parallelStream().forEach { article =>
        val converter = new SastToTexConverter(
          project,
          article.sourceDoc.file,
          article.sourceDoc.reporter,
          preprocessed.directory,
          preprocessed.labels,
        )

        val resultContext =
          converter.convert(article.content)(ConversionContext(()))

        val headerres = converter.articleHeader(article, resultContext)

        val content = headerres.data ++ resultContext.data

        val articlename = Format.canonicalName(article)

        val outputdir = project.outputdir / "pdfs"
        outputdir.createDirectories()

        val targetfile = outputdir / s"$articlename.pdf"

        val jobname    = targetfile.nameWithoutExtension(includeAll = false)
        val temptexdir = project.cacheDir / s"$articlename.outdir"
        temptexdir.createDirectories()
        val temptexfile = temptexdir / (jobname + ".tex")

        project.bibfile.foreach(_.copyTo(temptexdir / "bibliography.bib")(copyOptions = CopyOptions(overwrite = true)))

        val templateSettings =
          project.config.definitions ++ article.header.attributes.named ++ List(
            Some("template content"  -> content.iterator.mkString("\n")),
            project.bibfile.map( _ => "bibliography path" -> "bibliography.bib")
          ).flatten ++ resultContext.features.toList.map(s => s"feature $s" -> "")

        val documentString: String =
          ConvertTemplate.fillTemplate(
            project,
            preprocessed.directory,
            article.named.get("texTemplate").orElse(project.config.texTemplate).get,
            templateSettings
          )

        val successfile = temptexdir / "lastsuccess.sha1"
        val scripthash  = Hashes.sha1hex(documentString)
        if successfile.exists && successfile.contentAsString == scripthash then ()
        else
          temptexfile.write(documentString)
          val res = Latexmk.latexmk(temptexdir, jobname, temptexfile)
          targetfile.delete(swallowIOExceptions = true)
          res.foreach(_.linkTo(targetfile))
          if res.isDefined then successfile.write(scripthash)
      }

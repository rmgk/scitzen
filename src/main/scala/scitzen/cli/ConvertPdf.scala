package scitzen.cli

import scitzen.bibliography.BibDB
import scitzen.contexts.ConversionContext
import scitzen.extern.{Hashes, ImageConverter, Latexmk}
import scitzen.generic.{ArticleDirectory, Project}
import scitzen.outputs.SastToTexConverter

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, StandardCopyOption, StandardOpenOption}
import scala.jdk.CollectionConverters.*

object ConvertPdf:
  implicit val charset: Charset = StandardCharsets.UTF_8

  def convertToPdf(
      project: Project,
      preprocessed: ArticleDirectory,
    bibDB: BibDB,
  ): Unit =
    preprocessed.fullArticles
      .filter(_.header.attributes.named.contains("texTemplate"))
      .asJava.parallelStream().forEach { article =>
        val converter = new SastToTexConverter(
          project,
          article.article,
          preprocessed,
          bibDB
        )

        val resultContext =
          converter.convert(article.body)(ConversionContext(()))

        val headerres = converter.articleHeader(article, resultContext)

        val content = headerres.data ++ resultContext.data

        val articlename = Format.canonicalName(article.header)


        Files.createDirectories(project.outputdirPdf)

        val targetfile = project.outputdirPdf.resolve(s"$articlename.pdf")

        val jobname    = ImageConverter.nameWithoutExtension(targetfile)
        val temptexdir = project.cacheDir.resolve(s"$articlename.outdir")
        Files.createDirectories(temptexdir)
        val temptexfile = temptexdir.resolve(jobname + ".tex")

        project.bibfile.foreach { bf =>
          Files.copy(
            bf.absolute,
            temptexdir.resolve("bibliography.bib"),
            StandardCopyOption.REPLACE_EXISTING,
            StandardCopyOption.COPY_ATTRIBUTES
          )
        }
        if Files.isRegularFile(project.bibfileDBLPcache.absolute) then
          Files.write(
            temptexdir.resolve("bibliography.bib"),
            Files.readAllBytes(project.bibfileDBLPcache.absolute),
            StandardOpenOption.CREATE,
            StandardOpenOption.APPEND
          )
          ()

        val templateSettings =
          project.config.definitions ++ article.header.attributes.named ++ List(
            Some("template content" -> content.iterator.mkString("\n")),
            project.bibfile.map(_ => "bibliography path" -> "bibliography.bib")
          ).flatten ++ resultContext.features.toList.map(s => s"feature $s" -> "")

        val documentString: String =
          ConvertTemplate.fillTemplate(
            project,
            preprocessed,
            article.named.get("texTemplate").orElse(project.config.texTemplate).get,
            templateSettings
          )

        val successfile = temptexdir.resolve("lastsuccess.sha1")
        val scripthash  = Hashes.sha1hex(documentString)
        if Files.exists(successfile) && Files.readString(successfile) == scripthash then ()
        else
          Files.writeString(temptexfile, documentString)
          val res = Latexmk.latexmk(temptexdir, jobname, temptexfile)
          Files.deleteIfExists(targetfile)
          res.foreach(r => Files.createLink(targetfile, r))
          if res.isDefined then
            Files.writeString(successfile, scripthash)
            ()
      }

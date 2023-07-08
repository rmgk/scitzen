package scitzen.cli

import scitzen.contexts.ConversionContext
import scitzen.extern.{Hashes, ImageConverter, Latexmk}
import scitzen.outputs.SastToTexConverter
import scitzen.sast.{Attribute, Attributes}

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, StandardCopyOption, StandardOpenOption}
import scala.jdk.CollectionConverters.*

object ConvertPdf:
  implicit val charset: Charset = StandardCharsets.UTF_8

  def convertToPdf(
      anal: ConversionAnalysis
  ): Unit =

    def project = anal.project

    anal.selected.filter(ta => ta.flags.tex || ta.header.attributes.plain("texTemplate").isDefined)
      .asJava.parallelStream().forEach { titled =>
        val converter = new SastToTexConverter(
          titled.article.ref,
          anal,
          Attributes(project.config.attrs.raw ++ titled.header.attributes.raw),
          flags = titled.flags
        )

        val resultContext =
          converter.convertSastSeq(ConversionContext(()), titled.article.sast)

        val content = resultContext.data

        val articlename = Format.canonicalName(titled.header)

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
          Attributes(project.config.attrs.raw ++ titled.header.attributes.raw ++
            resultContext.features.map(s => Attribute(s"feature $s", "")) ++
            project.bibfile.map(_ => Attribute("bibliography path", "bibliography.bib")).toList :+
            Attribute("template content", content.mkString("\n")))

        val documentString: String =
          ConvertTemplate.fillTemplate(
            project,
            anal.directory,
            templateSettings.plain("texTemplate").flatMap(
              titled.article.doc.resolve
            ).orElse(Some(project.pdfTemplatePath)),
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

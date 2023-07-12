package scitzen.cli

import de.rmgk.delay.Async
import scitzen.contexts.{ConversionContext, FileDependency}
import scitzen.extern.{Hashes, Latexmk}
import scitzen.generic.ProjectPath
import scitzen.outputs.SastToTexConverter
import scitzen.resources.Filetype
import scitzen.sast.{Attribute, Attributes}

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, StandardCopyOption, StandardOpenOption}
import scala.jdk.CollectionConverters.*

object ConvertPdf:
  implicit val charset: Charset = StandardCharsets.UTF_8

  case class PdfTask(dependencies: List[FileDependency], task: Async[Any, Unit])

  def convertToPdf(
      anal: ConversionAnalysis
  ): List[PdfTask] =

    def project = anal.project

    anal.selected.filter(ta => ta.flags.tex || ta.header.attributes.plain("texTemplate").isDefined)
      .map: titled =>

        val articlename = Format.canonicalName(titled.header)

        Files.createDirectories(project.outputdirPdf)

        val targetfile = project.outputdirPdf.resolve(s"$articlename.pdf")

        val jobname    = Filetype.nameWithoutExtension(targetfile)
        val temptexdir = project.cacheDir.resolve(s"$articlename.outdir")
        Files.createDirectories(temptexdir)
        val temptexfile = temptexdir.resolve(jobname + ".tex")

        val converter = new SastToTexConverter(
          ::(titled.article.ref, Nil),
          anal,
          Attributes(project.config.attrs.raw ++ titled.header.attributes.raw),
          ProjectPath(project, temptexdir),
          flags = titled.flags,
        )

        val resultContext =
          converter.convertSastSeq(ConversionContext(()), titled.article.sast)

        val content = resultContext.data

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

        PdfTask(
          resultContext.fileDependencies, {
            val scripthash  = Hashes.sha1hex(documentString)
            val successfile = temptexdir.resolve("lastsuccess.sha1")
            if Files.exists(successfile) && Files.readString(successfile) == scripthash then Async(())
            else
              Async:
                Files.writeString(temptexfile, documentString)
                val res = Latexmk.latexmk(temptexdir, jobname, temptexfile).bind
                Files.deleteIfExists(targetfile)
                res.foreach(r => Files.createLink(targetfile, r))
                if res.isDefined then
                  Files.writeString(successfile, scripthash)
                  ()
          }
        )

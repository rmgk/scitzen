package scitzen.cli

import de.rmgk.delay.Async
import scitzen.contexts.{ConversionContext, TargetedFileDependency}
import scitzen.extern.{Hashes, Latexmk}
import scitzen.project.{ProjectPath, TitledArticle}
import scitzen.outputs.SastToTexConverter
import scitzen.resources.Filetype
import scitzen.sast.{Attribute, Attributes}

import java.io.BufferedOutputStream
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, StandardOpenOption}
import scala.jdk.CollectionConverters.*
import scala.util.Using

object ConvertPdf:
  implicit val charset: Charset = StandardCharsets.UTF_8

  case class PdfTask(dependencies: List[TargetedFileDependency], task: Async[Any, Unit])

  def convertToPdf(
      anal: ConversionAnalysis,
      selected: List[TitledArticle]
  ): List[PdfTask] =

    def project = anal.project

    selected.filter(ta => ta.flags.tex || ta.header.attributes.plain("texTemplate").isDefined)
      .map: titled =>

        val articlename = Format.canonicalName(titled.header, ".pdf")

        Files.createDirectories(project.outputdirPdf)

        val targetfile = project.outputdirPdf.resolve(s"$articlename")

        val jobname    = Filetype.nameWithoutExtension(targetfile)
        val temptexdir = project.cacheDir.resolve(s"$articlename.outdir")
        Files.createDirectories(temptexdir)
        val temptexfile = temptexdir.resolve(jobname + ".tex")

        val converter = new SastToTexConverter(
          ::(titled.article.ref, Nil),
          anal,
          Attributes(project.config.attrs.raw ++ titled.header.attributes.raw),
          flags = titled.flags,
        )

        val resultContext =
          converter.convertSastSeq(ConversionContext(()), titled.article.sast)

        val content = resultContext.data

        val target = temptexdir.resolve("bibliography.bib")

        Using(
          new BufferedOutputStream(Files.newOutputStream(
            target,
            StandardOpenOption.CREATE,
            StandardOpenOption.TRUNCATE_EXISTING
          ))
        ): bo =>
          resultContext.usedCitations.foreach: used =>
            anal.bib.bibletmap.getOrElse(used.id, Nil).foreach: biblet =>
              biblet.inputstream.transferTo(bo)
        .get

        val templateSettings =
          Attributes(project.config.attrs.raw ++ titled.header.attributes.raw ++
            resultContext.features.map(s => Attribute(s"feature $s", "")) ++
            Option.when(resultContext.usedCitations.nonEmpty)(
              Attribute("bibliography path", "bibliography.bib")
            ).toList :+
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
          resultContext.fileDependencies.map(fd => TargetedFileDependency(fd, ProjectPath(project, temptexfile))), {
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

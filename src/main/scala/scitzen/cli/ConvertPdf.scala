package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}

import cats.data.Chain
import cats.implicits._
import scitzen.extern.TexTikz.latexmk
import scitzen.generic.{ConversionContext, ImageConverter, ParsedDocument, Project}
import scitzen.outputs.{SastToSastConverter, SastToTexConverter, TexPages}

object ConvertPdf {
  implicit val charset: Charset = StandardCharsets.UTF_8

  def convertToPdf(project: Project): Unit = {

    val documents: List[ParsedDocument] = project.documentManager.documents

    scribe.info(s"found ${documents.size} posts")

    project.outputdir.createDirectories()

    val dm   = project.documentManager
    val main = dm.byPath(project.main.get)

    val cacheDir = project.cacheDir

    cacheDir.createDirectories()

    val preConversionContext = ConversionContext(
      Chain.empty[String])

    def preprocConverter(doc: ParsedDocument): SastToSastConverter = {
      new SastToSastConverter(
        project,
        doc.file,
        doc.reporter,
        new ImageConverter(project, "pdf")
        )
    }

    val preprocessed = preprocConverter(main.parsed).convertSeq(main.sast)(preConversionContext)


    val resultContext = new SastToTexConverter(
      project,
      main.parsed.file.parent,
      main.parsed.reporter,
      preprocConverter
      ).convert(
      preprocessed.data.toList
      )(preprocessed)

    val content = resultContext.data

    import scala.jdk.CollectionConverters._
    resultContext.tasks.asJava.parallelStream().forEach { ct =>
      ct.run()
    }


    val targetfile = project.outputdir / "output.pdf"


    val bibliography = dm.fulldocs.collectFirstSome { pd =>
      pd.analyzed.named.get("bibliography").map(s => pd.parsed.file.parent / s.trim)
    }.map(_.pathAsString)
    scribe.info(s"bib is $bibliography")
    val authors = dm.analyzed.collectSomeFold(_.named.get("authors"))

    val jobname     = targetfile.nameWithoutExtension(includeAll = false)
    val temptexfile = cacheDir / (jobname + ".tex")
    val temptexdir  = cacheDir / "tex"
    temptexfile.write(TexPages.wrap(content, authors, "memoir", bibliography))
    latexmk(temptexdir, jobname, temptexfile).copyTo(targetfile, overwrite = true)
  }

}

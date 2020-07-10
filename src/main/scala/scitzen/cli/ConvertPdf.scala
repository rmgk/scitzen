package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}

import better.files._
import cats.data.Chain
import cats.implicits._
import scitzen.extern.ImageConverter
import scitzen.extern.TexTikz.latexmk
import scitzen.generic.{ConversionContext, ParsedDocument, Project, Sast}
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
      Chain.empty[String]
    )

    def preprocConverter(doc: ParsedDocument): SastToSastConverter = {
      new SastToSastConverter(
        project,
        doc.file,
        doc.reporter,
        new ImageConverter(project, "pdf", List("svg"))
      )
    }

    val docsCtx = dm.fulldocs.foldLeft(preConversionContext.ret(Map.empty[File, List[Sast]])) { (ctx, doc) =>
      SastToSastConverter.preprocessRecursive(doc, ctx, dm, preprocConverter)
    }

    val resultContext = new SastToTexConverter(
      project,
      main.parsed.file.parent,
      main.parsed.reporter,
      docsCtx.data
    ).convert(
      docsCtx.data(main.parsed.file)
    )(docsCtx)

    val content = resultContext.data

    import scala.jdk.CollectionConverters._
    resultContext.tasks.asJava.parallelStream().forEach { ct =>
      ct.run()
    }

    val targetfile = project.outputdir / "output.pdf"

    def fileFromParam(param: String) = {
      dm.fulldocs.collectFirstSome { pd =>
        pd.analyzed.named.get(param).map(s => pd.parsed.file.parent / s.trim)
      }
    }

    val bibliography = fileFromParam("bibliography").map(_.pathAsString)
    scribe.debug(s"bib is $bibliography")
    val authors = dm.analyzed.collectFoldSome(_.named.get("authors"))

    val macros = fileFromParam("texMathMacroFile").map(_.contentAsString).getOrElse("")

    val jobname     = targetfile.nameWithoutExtension(includeAll = false)
    val temptexfile = cacheDir / (jobname + ".tex")
    val temptexdir  = cacheDir / "tex"
    temptexfile.write(TexPages.wrap(content, authors, "memoir", bibliography, macros))
    latexmk(temptexdir, jobname, temptexfile).copyTo(targetfile, overwrite = true)
  }

}

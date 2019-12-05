package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.data.Chain
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import scitzen.extern.TexTikz.latexmk
import scitzen.generic.{ConversionContext, GenIndexPage, ImageResolver, ParsedDocument, Project}
import scitzen.outputs.{SastToTexConverter, TexPages}

object ConvertPdf {
  implicit val charset: Charset = StandardCharsets.UTF_8
  val optSource  : Opts[Path]         = Opts.argument[Path](metavar = "path")


  val command: Command[Unit] = Command(name = "pdf",
                                       header = "Convert Scim to PDF.") {
    optSource.map { sourcedirRel =>
      //val sync = syncFileRelOption.map2(syncPos)((f, p) => File(f) -> p)
      Project.fromSource(File(sourcedirRel)).foreach { project =>
        convertToPdf(project)
      }
    }
  }



  def convertToPdf(project: Project): Unit = {

    val documents: List[ParsedDocument] = project.documents

    scribe.info(s"found ${documents.size} posts")

    project.outputdir.createDirectories()

    val singleFile = project.singleSource.isDefined

    val dm = project.documentManager

    val cacheDir = project.cacheDir

    cacheDir.createDirectories()

    val preConversionContext = ConversionContext(
      Chain.empty[String], images = new ImageResolver(project, project.outputdir))

    val resultContext = new SastToTexConverter(
      project,
      project.singleSource.fold(project.root)(_.parent),
      if (singleFile) Some(dm.byPath(project.singleSource.get)) else None,
      numbered = singleFile).convert(
      if (singleFile) dm.byPath(project.singleSource.get).sdoc.blocks.toList else GenIndexPage.makeIndex(dm, project)
      )(preConversionContext)

    val content = resultContext.data


    val targetfile = project.outputdir / "output.pdf"

    val bibliography = dm.documents.collectFirstSome{ pd =>
      pd.sdoc.named.get("bibliography").map(s => pd.file.parent/s.trim)}.map(_.pathAsString)
    scribe.info(s"bib is $bibliography")
    val authors = dm.documents.collectSomeFold(_.sdoc.named.get("authors"))

    val jobname = targetfile.nameWithoutExtension(includeAll = false)
    val temptexfile = cacheDir / (jobname + ".tex")
    val temptexdir = cacheDir / "tex"
    temptexfile.write(TexPages.wrap(content, authors,
                                    if (singleFile) "thesis" else "memoir", bibliography))
    latexmk(temptexdir, jobname, temptexfile).copyTo(targetfile, overwrite = true)
  }

}

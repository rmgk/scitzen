package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.data.Chain
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import scitzen.extern.TexTikz.latexmk
import scitzen.generic.{ConversionContext, ParsedDocument, Project}
import scitzen.outputs.{SastToTexConverter, TexPages}

object ConvertPdf {
  implicit val charset: Charset = StandardCharsets.UTF_8
  val optSource  : Opts[Path]         = Opts.argument[Path](metavar = "path")


  val command: Command[Unit] = Command(name = "pdf",
                                       header = "Convert Scim to PDF.") {
    optSource.map { sourcePath =>
      //val sync = syncFileRelOption.map2(syncPos)((f, p) => File(f) -> p)
      Project.fromSource(File(sourcePath)).foreach { project =>
        convertToPdf(project)
      }
    }
  }



  def convertToPdf(project: Project): Unit = {

    val documents: List[ParsedDocument] = project.documentManager.documents

    scribe.info(s"found ${documents.size} posts")

    project.outputdir.createDirectories()

    val dm = project.documentManager
    val main = dm.byPath(project.main.get)

    val cacheDir = project.cacheDir

    cacheDir.createDirectories()

    val preConversionContext = ConversionContext(
      Chain.empty[String])

    val resultContext = new SastToTexConverter(
      project,
      main.parsed.file.parent,
      main.parsed.reporter
      ).convert(
      main.parsed.sast
      )(preConversionContext)

    val content = resultContext.data


    val targetfile = project.outputdir / "output.pdf"



    val bibliography = None
    //  dm.analyzed.collectFirstSome { pd =>
    //  pd.named.get("bibliography").map(s => pd.file.parent / s.trim)
    //}.map(_.pathAsString)
    scribe.info(s"bib is $bibliography")
    val authors = dm.analyzed.collectSomeFold(_.named.get("authors"))

    val jobname = targetfile.nameWithoutExtension(includeAll = false)
    val temptexfile = cacheDir / (jobname + ".tex")
    val temptexdir = cacheDir / "tex"
    temptexfile.write(TexPages.wrap(content, authors, "memoir", bibliography))
    latexmk(temptexdir, jobname, temptexfile).copyTo(targetfile, overwrite = true)
  }

}

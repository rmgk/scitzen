package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import scitzen.extern.TexTikz.latexmk
import scitzen.generic.{GenIndexPage, ImageResolver, ParsedDocument, Project}
import scitzen.outputs.{SastToTexConverter, TexPages}

object ConvertPdf {
  implicit val charset: Charset = StandardCharsets.UTF_8
  val optSource  : Opts[Path]         = Opts.argument[Path](metavar = "path")


  val command: Command[Unit] = Command(name = "pdf",
                                          header = "Convert Scim to HTML.") {
    optSource.map { sourcedirRel =>
      //val sync = syncFileRelOption.map2(syncPos)((f, p) => File(f) -> p)
      Project.fromSource(File(sourcedirRel)).foreach { project =>
        scribe.info(project.toString)
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

    val imageResolver = ImageResolver.fromDM(project.documentManager, project.cacheDir, keepName = true)

    val content = new SastToTexConverter(project.documentManager,
                                         numbered = singleFile,
                                         root = project.root,
                                         imageResolver = imageResolver).convert(
      if (singleFile) dm.byPath(project.singleSource.get).sdoc.blocks.toList else GenIndexPage.makeIndex(dm)
      )

    val cacheDir = project.cacheDir

    cacheDir.createDirectories()
    imageResolver.copyToTarget(cacheDir)

    val targetfile = project.outputdir / "output.pdf"

    val bibliography = dm.documents.collectFirstSome{ pd =>
      pd.sdoc.named.get("bibliography").map(s => pd.file.parent/s.trim)}.map(_.pathAsString)
    val authors = dm.documents.collectSomeFold(_.sdoc.named.get("authors"))

    val jobname = targetfile.nameWithoutExtension(includeAll = false)
    val temptexfile = cacheDir / (jobname + ".tex")
    val temptexdir = cacheDir / "tex"
    temptexfile.write(TexPages.wrap(content, authors,
                                    if (singleFile) "thesis" else "memoir", bibliography))
    latexmk(temptexdir, jobname, temptexfile).copyTo(targetfile, overwrite = true)
  }

}

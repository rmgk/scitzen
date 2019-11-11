package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.implicits._
import com.monovore.decline.Visibility.Partial
import com.monovore.decline.{Command, Opts}
import scitzen.extern.TexTikz.latexmk
import scitzen.generic.{DocumentDiscovery, DocumentManager, GenIndexPage, ImageResolver, NLP, ParsedDocument, Project, Sdoc}
import scitzen.outputs.{HtmlPages, HtmlToc, SastToHtmlConverter, SastToTexConverter, TexPages}
import scitzen.parser.MacroCommand.Cite

import scala.collection.mutable
import scala.util.Try


object Convert {

  implicit val charset: Charset = StandardCharsets.UTF_8

  val optSource  : Opts[Path]         = Opts.argument[Path](metavar = "path")

  val optSyncFile: Opts[Option[Path]] = Opts.option[Path]("sync-file", metavar = "file",
                                                          visibility = Partial,
                                                          help = "file to show in output").orNone
  val optSyncPos : Opts[Option[Int]]  = Opts.option[Int]("sync-position", metavar = "integer",
                                                         visibility = Partial,
                                                         help = "character offset to show in output").orNone

  // loading ressource statically allows Graal AOT to inline on build
  val stylesheet: Array[Byte] = {
    Resource.asStream("scitzen.css").fold(File("scitzen.css").byteArray)(_.byteArray)
  }



  val command: Command[Unit] = Command(name = "convert",
                                       header = "Convert Scim to HTML/PDF.") {
    (optSource, optSyncFile, optSyncPos).mapN {
      (sourcedirRel, syncFileRelOption, syncPos) =>

        val sync = syncFileRelOption.map2(syncPos)((f, p) => File(f) -> p)

        Project.fromSource(File(sourcedirRel)).foreach { project =>
          scribe.info(project.toString)
          convertToHtml(project, sync)
        }
    }
  }


  def convertToPdf(sourcefile: File, targetfile: File, cacheDir: File): Unit = {
    val dd = DocumentDiscovery(List(sourcefile))

    val documents: List[ParsedDocument] = dd.sourceFiles.map(ParsedDocument.apply)

    scribe.info(s"found ${documents.size} posts")

    val dm = DocumentManager.resolveIncludes(new DocumentManager(documents))
    val ir = ImageResolver.fromDM(dm, cacheDir, keepName = false)

    val singleFile = sourcefile.isRegularFile

    val content = new SastToTexConverter(dm,
                                         numbered = singleFile,
                                         root = if (singleFile) sourcefile.parent else sourcefile,
                                         imageResolver = ir).convert(
      if (singleFile) dm.byPath(sourcefile).blocks.toList else GenIndexPage.makeIndex(dm)
    )


    cacheDir.createDirectories()
    ir.copyToTarget(cacheDir)

    val bibliography = dm.documents.collectFirstSome{ pd =>
      pd.sdoc.named.get("bib").map(s => pd.file.parent/s.trim)}.map(_.pathAsString)
    val authors = dm.documents.collectSomeFold(_.sdoc.named.get("authors"))

    val jobname = targetfile.nameWithoutExtension(includeAll = false)
    val temptexfile = cacheDir / (jobname + ".tex")
    val temptexdir = cacheDir / "tex"
    temptexfile.write(TexPages.wrap(content, authors,
                                    if (singleFile) "thesis" else "memoir", bibliography))
    latexmk(temptexdir, jobname, temptexfile).copyTo(targetfile, overwrite = true)
  }


  def convertToHtml(project: Project, sync: Option[(File, Int)]): Unit = {

    val documents: List[ParsedDocument] = project.documents

    scribe.info(s"found ${documents.size} posts")

    val dm = project.documentManager

    project.outputdir.createDirectories()

    val cssfile = project.outputdir / "scitzen.css"
    cssfile.writeByteArray(stylesheet)

    val imageResolver = ImageResolver.fromDM(dm, project.cacheDir, keepName = true)

    val scitzenconfdir = project.projectDir
    val nlp = if (scitzenconfdir.isDirectory) Some(NLP.loadFrom(scitzenconfdir, dm)) else None


    val (bibEntries: Seq[Bibliography.BibEntry], biblio) = project.singleSource match {
      case None             => (Nil, Map[String, String]())
      case Some(sourcefile) =>
        val doc = dm.byPath(sourcefile)
        val bibPath = doc.sdoc.named.get("bibliography").map { p =>
          doc.file.parent./(p.trim)
        }
        val bib = bibPath.toList.flatMap(Bibliography.parse(project.cacheDir))
        val cited = dm.documents.flatMap {
          _.sdoc.analyzeResult.macros.filter(_.command == Cite)
           .flatMap(_.attributes.positional.flatMap(_.split(",")).map(_.trim))
        }.toSet
        val bibEntries = bib.filter(be => cited.contains(be.id)).sortBy(be => be.authors.map(_.family))
        val biblio = bibEntries.zipWithIndex.map { case (be, i) => be.id -> (i + 1).toString }.toMap
        bibEntries -> biblio
    }


    val katexmapfile = project.cacheDir / "katexmap.json"
    val katexMap = Try {
      upickle.default.read[mutable.Map[String, String]](katexmapfile.path)
    }.getOrElse(mutable.Map())




    def convertDoc(doc: ParsedDocument, postoutputdir: File): Unit = {

      val citations = if (bibEntries.isEmpty) Nil else {
        import scalatags.Text.all.{id, li, ol}
        import scalatags.Text.short._
        List(ol(bibEntries.zipWithIndex.map { case (be, i) => li(id := be.id, be.format) } ))
      }

      val converter = new SastToHtmlConverter(scalatags.Text,
                                              dm,
                                              imageResolver,
                                              biblio,
                                              doc.sdoc,
                                              doc.file,
                                              katexMap,
                                              sync,
                                              project)
      val toc = HtmlToc.tableOfContents(doc.sdoc.blocks, 2)
      val cssrelpath = postoutputdir.relativize(cssfile).toString
      val res = HtmlPages(cssrelpath).wrapContentHtml(converter.convert() ++ citations,
                                                      "fullpost",
                                                      toc,
                                                      doc.sdoc.language
                                                      .orElse(nlp.map(_.language(doc.sdoc)))
                                                      .getOrElse(""))
      val target = postoutputdir./(doc.file.nameWithoutExtension(false) + ".html")

      target.write(res)
    }


    project.singleSource match {
      case Some(sourcefile) =>
        val postoutput = project.outputdir
        postoutput.createDirectories()
        imageResolver.copyToTarget(postoutput)
        convertDoc(dm.byPath(sourcefile), postoutput)
      case None =>
        val postoutput = project.outputdir / "posts"
        postoutput.createDirectories()
        imageResolver.copyToTarget(postoutput)
        dm.documents.foreach {convertDoc(_, postoutput)}

        val sdoc = Sdoc(GenIndexPage.makeIndex(dm, reverse = true, nlp = nlp))
        val converter = new SastToHtmlConverter(scalatags.Text,
                                                dm,
                                                imageResolver,
                                                Map(),
                                                sdoc,
                                                project.root,
                                                katexMap,
                                                None,
                                                project)
        val toc = HtmlToc.tableOfContents(sdoc.blocks, 2)

        val res = HtmlPages(project.outputdir.relativize(cssfile).toString)
                  .wrapContentHtml(converter.convert(),
                                   "index",
                                   toc,
                                   sdoc.language.getOrElse(""))
        project.outputdir./("index.html").write(res)
    }

    if (katexMap.nonEmpty) {
      katexmapfile.parent.createDirectories()
      katexmapfile.write(upickle.default.write[mutable.Map[String, String]](katexMap, indent = 2))
    }

  }

}

package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.data.Chain
import cats.implicits._
import com.monovore.decline.Visibility.Partial
import com.monovore.decline.{Command, Opts}
import scitzen.generic._
import scitzen.outputs.{HtmlPages, HtmlToc, SastToHtmlConverter}

import scala.util.Try


object ConvertHtml {

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



  val command: Command[Unit] = Command(name = "html",
                                       header = "Convert Scim to HTML.") {
    (optSource, optSyncFile, optSyncPos).mapN {
      (sourcedirRel, syncFileRelOption, syncPos) =>

        val sync = syncFileRelOption.map2(syncPos)((f, p) => File(f) -> p)

        Project.fromSource(File(sourcedirRel)).foreach { project =>
          scribe.info(project.toString)
          convertToHtml(project, sync)
        }
    }
  }




  def convertToHtml(project: Project, sync: Option[(File, Int)]): Unit = {

    val documents: List[ParsedDocument] = project.documentManager.documents

    scribe.info(s"found ${documents.size} posts")

    val dm = project.documentManager

    project.outputdir.createDirectories()

    val cssfile = project.outputdir / "scitzen.css"
    cssfile.writeByteArray(stylesheet)

    val scitzenconfdir = project.nlpdir
    val nlp = if (scitzenconfdir.isDirectory) Some(NLP.loadFrom(scitzenconfdir, dm)) else None


    val (bibEntries: Seq[Bibliography.BibEntry], biblio) = (Nil, Map[String, String]())
    // project.singleSource match {
    //  case None             => (Nil, Map[String, String]())
    //  case Some(sourcefile) =>
    //    val doc = dm.byPath(sourcefile)
    //    val bibPath = doc.sdoc.named.get("bibliography").map { p =>
    //      doc.file.parent./(p.trim)
    //    }
    //    val bib = bibPath.toList.flatMap(Bibliography.parse(project.cacheDir))
    //    val cited = dm.documents.flatMap {
    //      _.sdoc.analyzeResult.macros.filter(_.command == Cite)
    //       .flatMap(_.attributes.positional.flatMap(_.split(",")).map(_.trim))
    //    }.toSet
    //    val bibEntries = bib.filter(be => cited.contains(be.id)).sortBy(be => be.authors.map(_.family))
    //    val biblio = bibEntries.zipWithIndex.map { case (be, i) => be.id -> (i + 1).toString }.toMap
    //    bibEntries -> biblio
    //}


    val katexmapfile = project.cacheDir / "katexmap.json"
    val initialKatexMap = Try {
      upickle.default.read[Map[String, String]](katexmapfile.path)
    }.getOrElse(Map())




    def convertDoc(doc: FullDoc, pathManager: HtmlPathManager, ctx: ConversionContext[_]): ConversionContext[_] = {

      val citations = if (bibEntries.isEmpty) Nil else {
        import scalatags.Text.all.{id, li, ol}
        import scalatags.Text.short._
        List(ol(bibEntries.zipWithIndex.map { case (be, i) => li(id := be.id, be.format) } ))
      }

      val analyzedDoc = doc.analyzed

      val converter = new SastToHtmlConverter(bundle = scalatags.Text,
                                              pathManager = pathManager,
                                              bibliography = biblio,
                                              sdoc = analyzedDoc,
                                              document = Some(doc.parsed),
                                              sync = sync,
                                              reporter = doc.parsed.reporter)
      val toc = HtmlToc.tableOfContents(analyzedDoc.blocks, 2)
      val cssrelpath = pathManager.outputDir.relativize(cssfile).toString
      val converted = converter.convert()(ctx)
      val res = HtmlPages(cssrelpath).wrapContentHtml(converted.data.toList ++ citations,
                                                      "fullpost",
                                                      toc,
                                                      analyzedDoc.language
                                                      .orElse(nlp.map(_.language(analyzedDoc)))
                                                      .getOrElse(""))
      val target = pathManager.translatePost(doc.parsed.file)
      target.write(res)
      converted
    }

    val preConversionContext =
      ConversionContext(Chain.empty[String],
                        katexMap = initialKatexMap
                        )

    val resultContext = project.documentManager.sources match {
      case List(sourcefile) =>
        val postoutput = project.outputdir
        postoutput.createDirectories()
        val doc         = dm.byPath(sourcefile)
        val pathManager = HtmlPathManager(doc.parsed.file,
                                          project,
                                          postoutput)
        val resctx      = convertDoc(doc, pathManager, preConversionContext)
        pathManager.copyResources(resctx.resourceMap)
        resctx
      case _             =>
        val postoutput = project.outputdir / "posts"
        postoutput.createDirectories()

        val outputCtx = preConversionContext

        val pathManager = HtmlPathManager(project.root,
                                  project,
                                  postoutput)

        val docsCtx = dm.fulldocs.foldLeft(outputCtx) { (ctx, doc) =>
          convertDoc(doc, pathManager.changeWorkingFile(doc.parsed.file), ctx).empty
        }


        val sdoc = AnalyzedDoc(GenIndexPage.makeIndex(dm, project, reverse = true, nlp = nlp), new SastAnalyzer(m => ""))
        val converter = new SastToHtmlConverter(bundle = scalatags.Text,
                                                pathManager = pathManager,
                                                bibliography = Map(),
                                                sdoc = sdoc,
                                                document = None,
                                                sync = None,
                                                reporter = m => "")
        val toc = HtmlToc.tableOfContents(sdoc.blocks, 2)

        val convertedCtx = converter.convert()(docsCtx)

        pathManager.copyResources(convertedCtx.resourceMap)

        val res = HtmlPages(project.outputdir.relativize(cssfile).toString)
                  .wrapContentHtml(convertedCtx.data.toList,
                                   "index",
                                   toc,
                                   sdoc.language.getOrElse(""))
        project.outputdir./("index.html").write(res)
        convertedCtx
    }


    if (resultContext.katexMap.nonEmpty) {
      katexmapfile.parent.createDirectories()
      katexmapfile.write(upickle.default.write[Map[String, String]](resultContext.katexMap, indent = 2))
    }

  }

}

package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.data.Chain
import cats.implicits._
import com.monovore.decline.Visibility.Partial
import com.monovore.decline.{Command, Opts}
import scitzen.generic.{ConversionContext, ExternalContentResolver, ExternalContentResolver2, GenIndexPage, NLP, ParsedDocument, Project, SastAnalyzes, Sdoc}
import scitzen.outputs.{HtmlPages, HtmlToc, SastToHtmlConverter}
import scitzen.parser.MacroCommand.Cite

import scala.collection.mutable
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

    val documents: List[ParsedDocument] = project.documents

    scribe.info(s"found ${documents.size} posts")

    val dm = project.documentManager

    project.outputdir.createDirectories()

    val cssfile = project.outputdir / "scitzen.css"
    cssfile.writeByteArray(stylesheet)

    val imageResolver = ExternalContentResolver.fromDM(dm, project.cacheDir, keepName = true)

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

      val preConversionContext = ConversionContext(Chain.empty[String], externalContentResolver = new ExternalContentResolver2(project, Nil))

      val converter = new SastToHtmlConverter(bundle = scalatags.Text,
                                              documentManager = dm,
                                              imageResolver = imageResolver,
                                              bibliography = biblio,
                                              sdoc = doc.sdoc,
                                              document = Some(doc),
                                              katexMap = katexMap,
                                              sync = sync,
                                              project = project)
      val toc = HtmlToc.tableOfContents(doc.sdoc.blocks, 2)
      val cssrelpath = postoutputdir.relativize(cssfile).toString
      val converted = converter.convert()(preConversionContext)
      val res = HtmlPages(cssrelpath).wrapContentHtml(converted.data.toList ++ citations,
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

              val preConversionContext = ConversionContext(Chain.empty[String], externalContentResolver = new ExternalContentResolver2(project, Nil))

        val sdoc = Sdoc(GenIndexPage.makeIndex(dm, reverse = true, nlp = nlp), new SastAnalyzes(m => ""))
        val converter = new SastToHtmlConverter(bundle = scalatags.Text,
                                                documentManager = dm,
                                                imageResolver = imageResolver,
                                                bibliography = Map(),
                                                sdoc = sdoc,
                                                document = None,
                                                katexMap = katexMap,
                                                sync = None,
                                                project = project)
        val toc = HtmlToc.tableOfContents(sdoc.blocks, 2)

              val converted = converter.convert()(preConversionContext)


        val res = HtmlPages(project.outputdir.relativize(cssfile).toString)
                  .wrapContentHtml(converted.data.toList,
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

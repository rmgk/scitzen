package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.implicits._
import com.monovore.decline.Visibility.Partial
import com.monovore.decline.{Command, Opts}
import scitzen.extern.Tex.latexmk
import scitzen.generic.{DocumentDiscovery, DocumentManager, GenIndexPage, ImageResolver, NLP, ParsedDocument, Sdoc}
import scitzen.outputs.{HtmlToc, SastToHtmlConverter, SastToTexConverter}

import scala.collection.mutable
import scala.util.Try

case class Project(root: File, singleSource: Option[File] = None) {
  val projectDir: File = root / Project.scitzenfolder
  val cacheDir  : File = projectDir / "cache"
  lazy val sources        : List[File]           = singleSource match {
    case None => Project.discoverSources(root)
    case Some(source) => List(source)
  }
  lazy val documents      : List[ParsedDocument] = sources.map(ParsedDocument.apply)
  lazy val documentManager: DocumentManager      = DocumentManager.resolveIncludes(new DocumentManager(documents))
  val outputdir: File = projectDir / "output"
}

object Project {
  val scitzenfolder: String = ".scitzen"
  def findRoot(source: File): Option[File] = {
    if (( source / scitzenfolder).isDirectory) Some(source)
    else source.parentOption.flatMap(findRoot)
  }
  def fromSource(file: File): Project = {
    val root = findRoot(file)
    val source =
      if (isScim(file)) Some(file)
      else if (file.isDirectory) {
        file.collectChildren(isScim, 1).toList match {
          case List(single) => Some(single)
          case other => None
        }
      } else None
    Project(root.getOrElse(file / "meh"), source)
  }

  def isScim(c: File): Boolean =
    c.isRegularFile &&
    c.extension(includeDot = false, toLowerCase = true).contains(fileEnding)

  val fileEnding = "scim"
  def discoverSources(source: File): List[File] = {
    import scala.collection.JavaConverters._
    source match {
      case f if f.isRegularFile => List(f)
      case f if f.isDirectory   =>
        f.collectChildren{ c =>
          isScim(c) &&
          !f.relativize(c).iterator().asScala.exists {_.toString.startsWith("_") }
        }.toList
    }
  }

}

object Convert {

  implicit val charset: Charset = StandardCharsets.UTF_8

  val optSource  : Opts[Path]         = Opts.argument[Path](metavar = "path")
  val optOutput  : Opts[Path]         = Opts.argument[Path](metavar = "path")
  val optCachedir: Opts[Option[Path]] = Opts.option[Path]("cache", metavar = "directory",
                                                          help = "Temoperary cache folder").orNone

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
                                       header = "Convert Scitzen documents into HTML.") {
    (optSource, optCachedir, optSyncFile, optSyncPos).mapN {
      (sourcedirRel, cachedirRel, syncFileRelOption, syncPos) =>

        val sync = syncFileRelOption.map2(syncPos)((f, p) => File(f) -> p)

        val project = Project.fromSource(File(sourcedirRel))

        scribe.info(project.toString)

        convertToHtml(project, sync)
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

    val postoutput = project.outputdir / "posts"
    postoutput.createDirectories()
    imageResolver.copyToTarget(postoutput)

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
          _.sdoc.analyzeResult.macros.filter(_.command == "cite")
           .flatMap(_.attributes.positional.flatMap(_.split(",")).map(_.trim))
        }.toSet
        val bibEntries = bib.filter(be => cited.contains(be.id)).sortBy(be => be.authors.map(_.family))
        val biblio = bibEntries.zipWithIndex.map { case (be, i) => be.id -> (i + 1).toString }.toMap
        bibEntries -> biblio
    }


    val katexmapfile = project.cacheDir / "katexmap.json"
    val katexMap = Try {
      scala.collection.mutable.Map(upickle.default.read[Seq[(String, String)]](katexmapfile.path): _*)
    }.getOrElse(mutable.Map())




    val cssrelpath = postoutput.relativize(cssfile).toString
    def convertDoc(doc: ParsedDocument): Unit = {

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
                                              sync)
      val toc = HtmlToc.tableOfContents(doc.sdoc.blocks, 2)
      val res = Pages(cssrelpath).wrapContentHtml(converter.convert() ++ citations,
                                                      "fullpost",
                                                      toc,
                                                      doc.sdoc.language
                                                      .orElse(nlp.map(_.language(doc.sdoc)))
                                                      .getOrElse(""))
      val target = postoutput./(doc.file.nameWithoutExtension(false) + ".html")

      target.write(res)
    }


    project.singleSource match {
      case Some(sourcefile) =>  convertDoc(dm.byPath(sourcefile))
      case None =>
        dm.documents.foreach {convertDoc}

        val sdoc = Sdoc(GenIndexPage.makeIndex(dm, reverse = true, nlp = nlp))
        val converter = new SastToHtmlConverter(scalatags.Text,
                                                dm,
                                                imageResolver,
                                                Map(),
                                                sdoc,
                                                project.root,
                                                katexMap,
                                                None)
        val toc = HtmlToc.tableOfContents(sdoc.blocks, 2)

        val res = Pages(project.outputdir.relativize(cssfile).toString)
                  .wrapContentHtml(converter.convert(),
                                   "index",
                                   toc,
                                   sdoc.language.getOrElse(""))
        project.outputdir./("index.html").write(res)
    }

    if (katexMap.nonEmpty) {
      katexmapfile.parent.createDirectories()
      katexmapfile.write(upickle.default.write[Seq[(String, String)]](katexMap.toSeq))
    }

  }

}

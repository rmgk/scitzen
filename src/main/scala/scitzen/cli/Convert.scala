package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import scitzen.extern.Tex.latexmk
import scitzen.generic.{DocumentManager, GenIndexPage, ImageResolver, NLP, Sdoc}
import scitzen.outputs.{HtmlToc, SastToHtmlConverter, SastToTexConverter}

import scala.collection.mutable
import scala.util.Try

object Convert {

  implicit val charset: Charset = StandardCharsets.UTF_8

  val optSource  : Opts[Path]         = Opts.argument[Path](metavar = "path")
  val optOutput  : Opts[Path]         = Opts.argument[Path](metavar = "path")
  val optCachedir: Opts[Option[Path]] = Opts.option[Path]("cache", metavar = "directory",
                                                help = "Temoperary cache folder").orNone

  val optSyncFile: Opts[Option[Path]] = Opts.option[Path]("sync-file", metavar = "file",
                                                          help = "file to show in output").orNone
  val optSyncPos : Opts[Option[Int]]  = Opts.option[Int]("sync-position", metavar = "integer",
                                                         help = "character offset to show in output").orNone

  // loading ressource statically allows Graal AOT to inline on build
  val stylesheet: Array[Byte] = {
    Resource.asStream("scitzen.css").fold(File("scitzen.css").byteArray)(_.byteArray)
  }



  val command: Command[Unit] = Command(name = "convert",
                                       header = "Convert Asciidoc documents into HTML.") {
    (optSource, optOutput, optCachedir, optSyncFile, optSyncPos).mapN {
      (sourcedirRel, targetdirRel, cachedirRel, syncFileRelOption, syncPos) =>

        val sync = syncFileRelOption.map2(syncPos)((f, p) => File(f) -> p)

        val sourcefile = File(sourcedirRel)
        val targetfile = File(targetdirRel)
        val cachedir = cachedirRel.fold {
          if (targetfile.extension.isEmpty) targetfile / ".scitzen-cache"
          else targetfile.sibling(targetfile.name + ".scitzen-cache")
        }(File(_))

        scribe.info(s"processing $sourcefile")
        scribe.info(s"to $targetfile")
        if (targetfile.extension().contains(".pdf"))
          convertToPdf(sourcefile, targetfile, cachedir)
        else if (targetfile.isDirectory || targetfile.notExists)
          convertToHtml(sourcefile, targetfile, cachedir, sync)
        else
          scribe.warn(s"unknown target $targetfile")
    }
  }

  def resolveIncludes(documentManager: DocumentManager): DocumentManager = {
    val includes = (for {
      docs <- documentManager.documents
      macrs <- docs.sdoc.analyzeResult.macros
      if macrs.command == "include"
      file = docs.file.parent / macrs.attributes.target
      if !documentManager.byPath.contains(file)
    } yield file).toSet
    if (includes.isEmpty) documentManager
    else {
      scribe.info(s"found includes: $includes")
      val newPF = includes.iterator.map(ParsedDocument.apply) ++: documentManager.documents
      resolveIncludes(new DocumentManager(newPF))
    }
  }

  def convertToPdf(sourcefile: File, targetfile: File, cacheDir: File): Unit = {
    val dd = DocumentDiscovery(List(sourcefile))

    val documents: List[ParsedDocument] = dd.sourceFiles.map(ParsedDocument.apply)

    scribe.info(s"found ${documents.size} posts")

    val dm = resolveIncludes(new DocumentManager(documents))
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


  def convertToHtml(sourcefile: File, targetdir: File, cacheDir: File, sync: Option[(File, Int)]): Unit = {
    targetdir.createDirectories()

    val dd = DocumentDiscovery(List(sourcefile))
    val documents: List[ParsedDocument] = dd.sourceFiles.map(ParsedDocument.apply)

    scribe.info(s"found ${documents.size} posts")

    val dm = resolveIncludes(new DocumentManager(documents))
    //val ir = ImageResolver.fromDM(dm)

    val singlefile = sourcefile.isRegularFile

    val postdir = if (singlefile) targetdir else targetdir / "posts"
    postdir.createDirectories()

    val cssfile = targetdir./("scitzen.css")
    cssfile.writeByteArray(stylesheet)
    val relcsspostpath = postdir.relativize(cssfile).toString

    val imageResolver = ImageResolver.fromDM(dm, cacheDir, keepName = true)

    imageResolver.copyToTarget(postdir)

    val scitzenconfdir = sourcefile/"scitzen"
    val nlp = if (scitzenconfdir.isDirectory) Some(NLP.loadFrom(scitzenconfdir, dm)) else None


    val (bibEntries, biblio) = if (singlefile) {
      val doc = dm.byPath(sourcefile)
      val bibPath = doc.sdoc.named.get("bibliography").map { p =>
        doc.file.parent./(p.trim)
      }
      val bib = bibPath.toList.flatMap(Bibliography.parse(cacheDir))
      val cited = dm.documents.flatMap{_.sdoc.analyzeResult.macros.filter(_.command == "cite")
                  .flatMap(_.attributes.positional.flatMap(_.split(",")).map(_.trim))}.toSet
      val bibEntries = bib.filter(be => cited.contains(be.id)).sortBy(be => be.authors.map(_.family))
      val biblio = bibEntries.zipWithIndex.map { case (be, i) => be.id -> (i + 1).toString }.toMap
      bibEntries -> biblio
    } else (Nil, Map[String, String]())


      val katexmapfile = cacheDir/("katexmap.json")
      val katexMap = Try {
        scala.collection.mutable.Map(upickle.default.read[Seq[(String, String)]](katexmapfile.path): _*)
      }.getOrElse(mutable.Map())



    def convertDoc(doc: ParsedDocument) = {

      val citations = if (bibEntries.isEmpty) Nil else {
        import scalatags.Text.all.{ol, li, id}
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
      val res = Pages(relcsspostpath).wrapContentHtml(converter.convert() ++ citations,
                                                      "fullpost",
                                                      toc,
                                                      doc.sdoc.language
                                                      .orElse(nlp.map(_.language(doc.sdoc)))
                                                      .getOrElse(""))
      val target = postdir./(doc.file.nameWithoutExtension(false) + ".html")

      target.write(res)
    }

    if (singlefile) convertDoc(dm.byPath(sourcefile))

    else {

      dm.documents.foreach {convertDoc}


      {
        val sdoc = Sdoc(GenIndexPage.makeIndex(dm, reverse = true, nlp = nlp))
        val converter = new SastToHtmlConverter(scalatags.Text,
                                                dm,
                                                imageResolver,
                                                Map(),
                                                sdoc,
                                                sourcefile,
                                                katexMap,
                                                None)
        val toc = HtmlToc.tableOfContents(sdoc.blocks, 2)

        val res = Pages(targetdir.relativize(cssfile).toString)
                  .wrapContentHtml(converter.convert(),
                                   "index",
                                   toc,
                                   sdoc.language.getOrElse(""))
        targetdir./("index.html").write(res)
      }
    }

    katexmapfile.parent.createDirectories()
    katexmapfile.write(upickle.default.write[Seq[(String, String)]](katexMap.toSeq))


  }

}

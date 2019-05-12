package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import scitzen.extern.Tex.latexmk
import scitzen.generic.{DocumentManager, ImageResolver}
import scitzen.outputs.SastToTexConverter







object Convert {

  implicit val charset: Charset = StandardCharsets.UTF_8

  val optSource = Opts.argument[Path](metavar = "path")
  val optOutput: Opts[Path] = Opts.argument[Path](metavar = "path")
  val optCachedir: Opts[Option[Path]] = Opts.option[Path]("cache", metavar = "directory",
                                                help = "Temoperary cache folder").orNone

  // loading ressource statically allows Graal AOT to inline on build
  val stylesheet: Array[Byte] = {
    Resource.asStream("scitzen.css").fold(File("scitzen.css").byteArray)(_.byteArray)
  }



  val command: Command[Unit] = Command(name = "convert",
                                       header = "Convert Asciidoc documents into HTML.") {
    (optSource, optOutput, optCachedir).mapN {
      (sourcedirRel, targetdirRel, cachedirRel) =>


        val sourcefile = File(sourcedirRel)
        val targetfile = File(targetdirRel)
        val cachedir = cachedirRel.fold {
          if (targetfile.isDirectory) targetfile / ".scitzen-cache"
          else targetfile.sibling(targetfile.name + ".scitzen-cache")
        }(File(_))

        scribe.info(s"processing $sourcefile")
        scribe.info(s"to $targetfile")

        convertToPdf(sourcefile, targetfile, cachedir)
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
    scribe.info(s"converting to $targetfile")

    val dm = resolveIncludes(new DocumentManager(documents))
    val ir = ImageResolver.fromDM(dm)

    val singleFile = sourcefile.isRegularFile

    val content = new SastToTexConverter(dm,
                                         numbered = singleFile,
                                         root = if (singleFile) sourcefile.parent else sourcefile,
                                         imageResolver = ir).convert(
      if (singleFile) dm.byPath(sourcefile).sast.toList else dm.makeIndex()
    )


    (cacheDir / "images").createDirectories()
    ir.substitutions.foreach{
      case (source, target) => source.copyTo(cacheDir / target, overwrite = true)
    }

    val bibliography = dm.documents.collectFirstSome{ pd =>
      pd.sdoc.named.get("bib").map(s => pd.file.parent/s.trim)}.map(_.pathAsString)
    val authors = dm.documents.collectSomeFold(_.sdoc.named.get("authors"))

    val jobname = targetfile.nameWithoutExtension
    val temptexfile = cacheDir / (jobname + ".tex")
    val temptexdir = cacheDir / "tex"
    temptexfile.write(TexPages.wrap(content, authors,
                                    if (singleFile) "thesis" else "memoir", bibliography))
    latexmk(temptexdir, jobname, temptexfile).copyTo(targetfile, overwrite = true)


  }

//    else {
//      val postdir = targetdir / "posts"
//      postdir.createDirectories()
//
//      var categoriesAndMore: Map[String, Set[String]] = Map()
//
//      for ((path, post) <- posts) {
//        try {
//          scribe.trace(s"converting s${post.title}")
//          val targetPath = postdir / asciiData.targetPath(path)
//          targetPath.parent.createDirectories()
//          val relpath = targetPath.parent.relativize(targetdir)
//
//          val sast = new SastConverter(includeBelow(sourcedir)).blockSequence(post.document.blocks)
//          val analyzed = SastAnalyzes.analyze(sast)
//          val content = new SastToHtmlConverter(scalatags.Text, Map(), analyzed, mutable.Map()).sastToHtml(
//            sast)
//
//          targetPath.write(Pages(s"$relpath/").wrapContentHtml(analyzed.language, content))
//        }
//        catch {
//          case e @ ParsingAnnotation(content, failure) =>
//            println(s"error while parsing $path")
//            val trace = failure
//            println(trace.msg)
//            println(trace.longMsg)
//            println(s"========\n$content\n========")
//            throw e
//          case NonFatal(other)                         =>
//            scribe.error(s"error while parsing $path")
//            throw other
//        }
//
//        // collect attributes
//        for (attribute <- post.attributes) {
//          val category = attribute._1
//          val categoryContents = categoriesAndMore.getOrElse(category, Set.empty)
//          categoriesAndMore = categoriesAndMore + (category -> (categoryContents + attribute._2))
//        }
//      }
//
////        categoriesAndMore.foreach{ c =>
////          println(c._1)
////          c._2.toSeq.sorted.foreach{d => println(s"  ${d.trim}")}
////
////        }
//
//
//      targetdir./("index.html").write(Pages().makeIndexOf(posts.map(_._2)))
//
//
//      copyImages(sourcedir, postdir)
//
//    }
  //def convertArticle(targetdir: File, sourcedir: File): Unit = {
  //  var start = System.nanoTime()
  //  def t = {
  //    val now = System.nanoTime()
  //    val diff = (now - start) / 1000000
  //    start = now
  //    diff
  //  }
  //  def infoT(str: String) = scribe.info(str)
  //  val post = new PostFolder(sourcedir.path).makePost(sourcedir.path)
  //  infoT(s"parsed $t")
  //  val sast = new SastConverter(includeBelow(sourcedir)).blockSequence(post.document.blocks)
  //  infoT(s"sastd $t")
  //  val analyzed = SastAnalyzes.analyze(sast)
  //  infoT(s"analyzed $t")
  //  val bibPath = bibRel.orElse(analyzed.named.get("bib").map(p => Paths.get(p.trim)))
  //                .map(bp => sourcedir.parent./(bp.toString))
  //  val bib = bibPath.toList.flatMap(Bibliography.parse)
  //  infoT(s"bibd $t")
  //  val cited = analyzed.macros.filter(_.command == "cite").flatMap(_.attributes.positional.flatMap(_.split(",")).map(_.trim)).toSet
  //  infoT(s"cited $t")
  //    val name = sourcedir.nameWithoutExtension
  //    val targetFile = targetdir / (name + ".tex")
  //    val bibName = bibPath.map { source =>
  //      val targetname = source.nameWithoutExtension.replaceAll("\\s", "_")
  //      source.copyTo(targetdir / (targetname + ".bib"), overwrite = true)
  //      targetname
  //    }
  //    val content = new SastToTexConverter(analyzed).sastToTex(sast)
  //
  //    targetFile.write(TexPages.wrap(content,
  //                                   analyzed,
  //                                   bibName))
  //    latexmk(targetdir/("output"), name, targetFile)
  //  makeTexelse {
  //    val bibEntries = bib.filter(be => cited.contains(be.id)).sortBy(be => be.authors.map(_.family))
  //    infoT(s"cited entries $t")
  //    val targetPath = targetdir / (sourcedir.nameWithoutExtension + ".html")
  //    val katexmapfile = sourcedir.sibling("katexmap.json")
  //    val katexmap = Try {
  //      scala.collection.mutable.Map(upickle.default.read[Seq[(String, String)]](katexmapfile.path): _*)
  //    }.getOrElse(mutable.Map())
  //    val biblio = bibEntries.zipWithIndex.map { case (be, i) => be.id -> (i + 1).toString }.toMap
  //    infoT(s"katex loaded $t")
  //    val content = frag(new SastToHtmlConverter(scalatags.Text, biblio, analyzed, katexmap).sastToHtml(sast),
  //                       ol(bibEntries.zipWithIndex.map { case (be, i) => li(id := be.id,
  //                                                                           be.format)
  //                       }))
  //    infoT(s"content generated $t")
  //    katexmapfile.write(upickle.default.write[Seq[(String, String)]](katexmap.toSeq))
  //
  //    targetPath.write(Pages().wrapContentHtml(analyzed.language, content))
  //    infoT(s"written $t")
  //    copyImages(sourcedir.parent, targetdir)
  //    infoT(s"imaged $t")
  //  }
  //}

  def allImages(sourcedir: File): List[File] = sourcedir.glob("**.{jpg,jpeg,webp,gif,png,svg}").toList

  def copyImages(sourcedir: File, targetdir: File): Unit = {
    //TODO: may want to copy all linked files, instead of all images

    allImages(sourcedir).foreach { sourceImage =>
      val relimage = sourcedir.relativize(sourceImage)
      val targetfile = targetdir / relimage.toString
      scribe.trace(s"copy $sourceImage to $targetfile")
      targetfile.parent.createDirectories()
      sourceImage.copyTo(targetfile, overwrite = true)
    }
  }
  def normalizeImages(sourcedir: File, targetdir: File): Map[String, String] = {
    var names = 0
    targetdir.createDirectories()
    allImages(sourcedir).map{ sourceImage =>
      val targetname = names.toString + sourceImage.extension.getOrElse("")
      names = names + 1
      val targetfile = targetdir / targetname
      scribe.trace(s"copy $sourceImage to $targetfile")
      sourceImage.copyTo(targetfile, overwrite = true)
      sourcedir.relativize(sourceImage).toString -> targetname
    }.toMap
  }
}

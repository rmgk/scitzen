package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}

import better.files._
import cats.data.Chain
import cats.implicits._
import org.jsoup.Jsoup
import scitzen.extern.{Bibliography, ImageConverter}
import scitzen.generic._
import scitzen.outputs.{SastToHtmlConverter, SastToSastConverter}
import scitzen.parser.MacroCommand.Cite

import scala.util.Try


object ConvertRevealPresentation {

  implicit val charset: Charset = StandardCharsets.UTF_8

  def convertToHtml(project: Project, sync: Option[(File, Int)]): Unit = {

    val documents: List[ParsedDocument] = project.documentManager.documents

    scribe.info(s"found ${documents.size} posts")

    val dm = project.documentManager

    project.outputdir.createDirectories()

    //val cssfile = project.outputdir / "scitzen.css"
    //cssfile.writeByteArray(stylesheet)

    //val scitzenconfdir = project.nlpdir
    //val nlp            = if (scitzenconfdir.isDirectory) Some(NLP.loadFrom(scitzenconfdir, dm)) else None

    val (bibEntries: Seq[Bibliography.BibEntry], biblio) = {
      val doc        = dm.fulldocs.find(_.analyzed.named.contains("bibliography"))
      val bibPath    = doc.flatMap(doc => doc.analyzed.named.get("bibliography").map { p =>
        doc.parsed.file.parent./(p.trim)
      })
      val bib        = bibPath.toList.flatMap(Bibliography.parse(project.cacheDir))
      val cited      = dm.analyzed.flatMap {
        _.analyzeResult.macros.filter(_.command == Cite)
         .flatMap(_.attributes.positional.flatMap(_.split(",")).map(_.trim))
      }.toSet
      val bibEntries = bib.filter(be => cited.contains(be.id)).sortBy(be => be.authors.map(_.family))
      val biblio     = bibEntries.zipWithIndex.map { case (be, i) => be.id -> (i + 1).toString }.toMap
      bibEntries -> biblio
    }


    val katexmapfile    = project.cacheDir / "katexmap.json"
    val initialKatexMap = Try {
      upickle.default.read[Map[String, String]](katexmapfile.path)
    }.getOrElse(Map())

    def conversionPreproc(doc: ParsedDocument): SastToSastConverter = {
      new SastToSastConverter(
        project,
        doc.file,
        doc.reporter,
        new ImageConverter(project, preferredFormat = "svg", unsupportedFormat = List("pdf"))
        )
    }


    def convertDoc(doc: FullDoc,
                   pathManager: HtmlPathManager,
                   ctx: ConversionContext[Map[File, List[Sast]]])
    : ConversionContext[Map[File, List[Sast]]] = {

      //val citations = if (bibEntries.isEmpty) Nil else {
      //  import scalatags.Text.all.{id, li, ol}
      //  import scalatags.Text.short._
      //  List(ol(bibEntries.zipWithIndex.map { case (be, i) => li(id := be.id, be.format) }))
      //}

      val analyzedDoc = doc.analyzed


      val preprocessed = SastToSastConverter.preprocessRecursive(
        doc, ctx,
        pathManager.project.documentManager, conversionPreproc).execTasks()

      val converter = new SastToHtmlConverter(bundle = scalatags.Text,
                                              pathManager = pathManager,
                                              bibliography = biblio,
                                              sdoc = analyzedDoc,
                                              sync = sync,
                                              reporter = doc.parsed.reporter,
                                              includeResolver = preprocessed.data)
      //val toc        = HtmlToc.tableOfContents(doc.sast, 2)
      //val cssrelpath = pathManager.outputDir.relativize(cssfile).toString


      val converted = converter.convertSeq(preprocessed.data(doc.parsed.file))(preprocessed)
      //val res       = HtmlPages(cssrelpath).wrapContentHtml(converted.data.toList ++ citations,
      //                                                      "fullpost",
      //                                                      toc,
      //                                                      analyzedDoc.language
      //                                                                 .orElse(nlp.map(_.language(analyzedDoc)))
      //                                                                 .getOrElse(""))
      //val target    = pathManager.translatePost(doc.parsed.file)
      //target.write(res)
      val templateDir = project.resolveUnchecked(project.root, project.config.revealTemplate.get)
      templateDir.copyTo(project.outputdir, overwrite = true)
      val template = templateDir./("index.html.template").contentAsString
      val content   = template.replace("<!-- SCITZEN PASTES STUFF HERE -->", scalatags.Text.all.frag(converted.data.toList: _*).render)
      val cleanContent = Jsoup.parse(content).outerHtml()
      project.outputdir./("index.html").write(cleanContent)

      converted.ret(preprocessed.data)
    }

    val preConversionContext =
      ConversionContext(Chain.empty[String],
                        katexMap = initialKatexMap)

    val sourcefile = project.config.main.flatMap(project.resolve(project.root, _)).get

    val postoutput = project.outputdir
    postoutput.createDirectories()
    val doc           = dm.byPath(sourcefile)
    val pathManager   = HtmlPathManager(sourcefile,
                                        project,
                                        postoutput)
    val resultContext = convertDoc(doc, pathManager, preConversionContext.ret(Map.empty))
    pathManager.copyResources(resultContext.resourceMap)


    resultContext.execTasks()


    if (resultContext.katexMap.nonEmpty) {
      katexmapfile.parent.createDirectories()
      katexmapfile.write(upickle.default.write[Map[String, String]](resultContext.katexMap, indent = 2))
    }

  }

}

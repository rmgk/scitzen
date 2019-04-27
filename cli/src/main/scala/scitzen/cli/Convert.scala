package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import scalatags.Text.attrs.id
import scalatags.Text.tags.frag
import scalatags.Text.implicits.stringAttr
import scalatags.Text.tags.SeqFrag
import scalatags.Text.tags.{li, ol}
import scitzen.converter.{HtmlConverter, SastToHtmlConverter}
import scitzen.parser.ParsingAnnotation
import scitzen.semantics.{SastAnalyzes, SastConverter}
import scribe.Logger

object Convert {

  val optSource = Opts.option[Path]("source", short = "s", metavar = "directory",
                                    help = "Directory containing Asciidoc source posts")
  val optOutput = Opts.option[Path]("output", short = "o", metavar = "directory",
                                    help = "Target output directory")
  val optBib = Opts.option[Path]("bibliography", short = "b", metavar = "file",
                                    help = "Bibliography").orNone
  // loading ressource statically allows Graal AOT to inline on build
  val stylesheet: Array[Byte] = {
    Resource.asStream("scitzen.css").fold(File("scitzen.css").byteArray)(_.byteArray)
  }


  val command: Command[Unit] = Command(name = "convert", header = "Convert Asciidoc documents into HTML.") {
    (optSource, optOutput, optBib).mapN {
      (sourcedirRel, targetdirRel, bibRel) =>
        implicit val charset: Charset = StandardCharsets.UTF_8

        import scribe.format._
        val myFormatter: Formatter = formatter"$message ($positionAbbreviated)"
        Logger.root.clearHandlers().withHandler(formatter = myFormatter, minimumLevel = Some(scribe.Level.Info)).replace()

        val sourcedir = File(sourcedirRel)
        val targetdir = File(targetdirRel)
        targetdir.createDirectories()
        scribe.info(s"processing $sourcedir")
        scribe.info(s"to $targetdir")

        if (sourcedir.isRegularFile) {
          val post = new PostFolder(sourcedir.path).makePost(sourcedir.path)
          val sast = SastConverter.blockSequence(post.document.blocks)
          val bib = bibRel.toList.flatMap(Bibliography.parse)
          val analyzed = SastAnalyzes.analyze(sast)
          val cited = analyzed.macros.filter(_.command == "cite").map(_.attributes.head.value).toSet
          val bibEntries = bib.filter(be => cited.contains(be.id)).sortBy(be => be.authors.map(_.family))
          val targetPath = targetdir/(sourcedir.nameWithoutExtension + ".html")
          val biblio = bibEntries.zipWithIndex.map{case (be, i) => be.id -> (i+1).toString }.toMap
          val content = frag(new SastToHtmlConverter(scalatags.Text, biblio, analyzed).sastToHtml(sast),
            ol(bibEntries.zipWithIndex.map{ case (be, i) => li(id:=be.id, be.format)}))

          targetPath.write(Pages().makeSastHtml(content))
          copyImages(sourcedir.parent, targetdir)
        }
        else if (sourcedir.isDirectory) {



          val asciiData = new PostFolder(sourcedir.path)

          val postdir = targetdir / "posts"
          postdir.createDirectories()

          def allAdocFiles(): List[File] = sourcedir.glob("**.adoc").toList

          val posts = allAdocFiles().map { f: File =>
            scribe.debug(s"parsing ${f.name}")
            f.path -> asciiData.makePost(f.path)
          }


          scribe.info(s"found ${posts.size} posts")
          scribe.info(s"converting to $targetdir")

          var categoriesAndMore: Map[String, Set[String]] = Map()

          for ((path, post) <- posts) {
            try {
              scribe.trace(s"converting s${post.title}")
              val targetPath = postdir / asciiData.targetPath(path)
              targetPath.parent.createDirectories()
              val relpath = targetPath.parent.relativize(targetdir)

              val content = new HtmlConverter(scalatags.Text, post).convert()
              targetPath.write(Pages(s"$relpath/").makePostHtml(post, content))
            }
            catch {
              case e @ ParsingAnnotation(content, failure) =>
                println(s"error while parsing $path")
                val trace = failure
                println(trace.msg)
                println(trace.longMsg)
                println(s"========\n$content\n========")
                throw e
            }

            // collect attributes
            for (attribute <- post.attributes) {
              val category = attribute._1
              val categoryContents = categoriesAndMore.getOrElse(category, Set.empty)
              categoriesAndMore = categoriesAndMore + (category -> (categoryContents + attribute._2))
            }
          }

//        categoriesAndMore.foreach{ c =>
//          println(c._1)
//          c._2.toSeq.sorted.foreach{d => println(s"  ${d.trim}")}
//
//        }


          targetdir./("index.html").write(Pages().makeIndexOf(posts.map(_._2)))


          copyImages(sourcedir, postdir)

        }
        scribe.info("copy static resources")
        (targetdir / "scitzen.css").writeByteArray(stylesheet)
    }
  }


  def copyImages(sourcedir: File, targetdir: File): Unit = {
    //TODO: may want to copy all linked files, instead of all images
    def allImages(): List[File] = sourcedir.glob("**.{jpg,jpeg,webp,gif,png,svg}").toList

    allImages().foreach { sourceImage =>
      val relimage = sourcedir.relativize(sourceImage)
      val targetfile = targetdir / relimage.toString
      scribe.trace(s"copy $sourceImage to $targetfile")
      targetfile.parent.createDirectories()
      sourceImage.copyTo(targetfile, overwrite = true)
    }
  }
}

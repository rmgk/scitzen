package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import de.rmgk.logging.{Level, Logger}
import scitzen.parser.ParsingAnnotation

object Convert {

  val Log = Logger(level = Level.Info)

  val optSource = Opts.option[Path]("source", short = "s", metavar = "directory",
                                    help = "Directory containing Asciidoc source posts")
  val optOutput = Opts.option[Path]("output", short = "o", metavar = "directory",
                                    help = "Target output directory")

  // loading ressource statically allows Graal AOT to inline on build
  val stylesheet: Array[Byte] = {
    Resource.asStream("scitzen.css").fold(File("scitzen.css").byteArray)(_.byteArray)
  }


  val command = Command(name = "convert", header = "Convert Asciidoc documents into HTML.") {
    (optSource, optOutput).mapN {
      (sourcedirRel, targetdirRel) =>

        implicit val charset: Charset = StandardCharsets.UTF_8

        val sourcedir = File(sourcedirRel)
        val targetdir = File(targetdirRel)
        targetdir.createDirectories()

        println(s"processing $sourcedir")
        println(s"to $targetdir")

        val asciiData = new PostFolder(sourcedir.path)

        val postdir = targetdir / "posts"
        postdir.createDirectories()

        def allAdocFiles(): List[File] = sourcedir.glob("**.adoc").toList

        val posts = allAdocFiles().map { f: File =>
          Log.trace(s"parsing ${f.name}")
          f.path -> asciiData.makePost(f.path)
        }


        Log.info(s"found ${posts.size} posts")
        Log.info(s"converting to $targetdir")

        var categoriesAndMore: Map[String, Set[String]] = Map()

        for ((path, post) <- posts) {
          try {
            Log.trace(s"converting s${post.title}")
            val targetPath = postdir / asciiData.targetPath(path)
            targetPath.parent.createDirectories()
            val relpath = targetPath.parent.relativize(targetdir)

            targetPath.write(Pages(s"$relpath/").makePostHtml(post))
          }
          catch {
            case e @ ParsingAnnotation(content, failure) =>
              println(s"error while parsing $path")
              val trace = failure.trace()
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

        Log.info("copy static resources")
        (targetdir / "scitzen.css").writeByteArray(stylesheet)


        //TODO: may want to copy all linked files, instead of all images
        def allImages(): List[File] = sourcedir.glob("**.{jpg,jpeg,webp,gif,png}").toList

        allImages().foreach { sourceImage =>
          val relimage = sourcedir.relativize(sourceImage)
          val targetfile = postdir / relimage.toString
          Log.trace(s"copy $sourceImage to $targetfile")
          targetfile.parent.createDirectories()
          sourceImage.copyTo(targetfile, overwrite = true)
        }

    }
  }


}
package pages

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import de.rmgk.logging.{Level, Logger}
import scitzen.converter.{AsciidocParser, Post}


object Convert {

  val Log = Logger(level = Level.Info)

  val optSource = Opts.option[Path]("source", short = "s", metavar = "directory",
                                    help = "Directory containing Asciidoc source posts")
  val optOutput = Opts.option[Path]("output", short = "o", metavar = "directory",
                                    help = "Target output directory")

  val command = Command(name = "convert", header = "Convert Asciidoc documents into HTML.") {
    (optSource, optOutput).mapN {
      (sourcedirRel, targetdirRel) =>

        implicit val charset: Charset = StandardCharsets.UTF_8

        val sourcedir = File(sourcedirRel)
        val targetdir = File(targetdirRel)
        targetdir.createDirectories()

        println(s"processing $sourcedir")
        println(s"to $targetdir")

        val asciiData = new AsciidocParser(sourcedir.path)

        val postdir = targetdir / "posts"
        postdir.createDirectories()

        def allAdocFiles(): List[File] = sourcedir.glob("**.adoc").toList

        def allPosts(): List[Post] = allAdocFiles().map { f: File =>
          Log.trace(s"parsing ${f.name}")
          asciiData.makePost(f.path)
        }


        val posts = allPosts
        Log.info(s"found ${posts.size} posts")
        Log.info(s"converting to $targetdir")

        var categoriesAndMore: Map[String, Set[String]] = Map()

        for (post <- posts) {
          Log.trace(s"converting s${post.title}")
          val targetPath = postdir / post.targetPath()
          targetPath.parent.createDirectories()
          val relpath = targetPath.parent.relativize(targetdir)
          targetPath.write(Pages(s"$relpath/").makePostHtml(post))

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


        targetdir./("index.html").write(Pages().makeIndexOf(posts))

        Log.info("copy static resources")

        (targetdir / "scitzen.css").writeBytes(new ResourceLoader().resourceBytes("scitzen.css"))


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

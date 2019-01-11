package scitzen.pages

import java.net.{JarURLConnection, URL}
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import scitzen.converter.{AsciiMedicImpl, Post}

import scala.collection.JavaConverters._
import scala.util.Try

class ResourceLoader() {

  val classloader: ClassLoader = getClass.getClassLoader
  val urls       : Seq[URL]   = classloader.getResources("META-INF/resources/webjars/").asScala.toList

  val assets: Seq[String] = urls.flatMap { url =>
    val jarUrlConnection = url.openConnection.asInstanceOf[JarURLConnection]
    jarUrlConnection.getJarFile.entries.asScala.filterNot(_.isDirectory).map(_.getRealName)
  }

  def findAsset(path: String): Option[String] = {
    assets.find(_.endsWith(path))
  }

  def resourceBytes(path: String): Iterator[Byte] = {
    Try {
      Resource.getAsStream(findAsset(path).get).buffered.bytes
    }.orElse(Try {
      Resource.getUrl()
      val resourcepath = Resource.getUrl()
      (File(resourcepath) / s"../../web/sass/main/stylesheets/$path").bytes
    }).get
  }

}

object Vitzen {


  val optSource = Opts.option[Path]("source", short = "s", metavar = "directory",
                                    help = "Directory containing Asciidoc source posts")
  val optOutput = Opts.option[Path]("output", short = "o", metavar = "directory",
                                    help = "Target output directory")

  val command = Command(name = "vitzen", header = "Convert Asciidoc documents into a webpage format.") {
    (optSource, optOutput).mapN {
      (sourcedirRel, targetdirRel) =>

        implicit val charset: Charset = StandardCharsets.UTF_8

        val sourcedir = File(sourcedirRel)
        val targetdir = File(targetdirRel)
        targetdir.createDirectories()

        println(s"processing $sourcedir")
        println(s"to $targetdir")


        (targetdir / "vitzen.css").writeBytes(new ResourceLoader().resourceBytes("vitzen.css"))

        val asciiData = new AsciiMedicImpl(sourcedir.path)

        val postdir = targetdir / "posts"
        postdir.createDirectories()

        def allAdocFiles(): List[File] = sourcedir.glob("**.adoc").toList

        def allPosts(): List[Post] = allAdocFiles().map { f: File =>
          println(f.name)
          asciiData.makePost(f.path)
        }


        val posts = allPosts
        println(s"found ${posts.size} posts")

        println(s"posting to $targetdir")

        for (post <- posts) {
          println(post.title)
          val targetPath = postdir / post.targetPath()
          targetPath.parent.createDirectories()
          val relpath = targetPath.parent.relativize(targetdir)
          targetPath.write(Pages(s"$relpath/").makePostHtml(post))
        }

        targetdir./("index.html").write(Pages().makeIndexOf(posts))

        //TODO: may want to copy all linked files, instead of all images
        def allImages(): List[File] = sourcedir.glob("**.{jpg,jpeg,webp,gif,png}").toList

        allImages().foreach { sourceImage =>
          val relimage = sourcedir.relativize(sourceImage)
          val targetfile = postdir / relimage.toString
          println(s"copied $sourceImage to $targetfile")
          targetfile.parent.createDirectories()
          sourceImage.copyTo(targetfile, overwrite = true)
        }

    }
  }


  def main(args: Array[String]): Unit = {
    run(args: _*)
  }

  def run(args: String*) = {
    command.parse(args) match {
      case Left(help)    =>
        println(help)
        sys.exit(0)
      case Right(result) => result
    }

  }
}

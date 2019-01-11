package scitzen.pages

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import com.monovore.decline.{Command, Opts}
import org.webjars.WebJarAssetLocator
import cats.implicits._
import scitzen.converter.{AsciiMedicImpl, Post}

object Vitzen {

  val resourceLocator = new WebJarAssetLocator()
  def resourceBytes(path: String): Iterator[Byte] =
    try {
      Resource.getAsStream(resourceLocator.getFullPath(path)).buffered.bytes
    } catch {
      case e: IllegalArgumentException =>
        val nonResourcelocation = File.currentWorkingDirectory / "target/web/sass/main/stylesheets/" / path
        if (nonResourcelocation.exists) nonResourcelocation.bytes
        else throw e
    }


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


        (targetdir / "vitzen.css").writeBytes(resourceBytes("vitzen.css"))
        (targetdir / "highlight.js").writeBytes(resourceBytes("highlight.pack.js"))


        val asciiData = new AsciiMedicImpl(sourcedir.path)

        val postdir = targetdir / "posts"
        postdir.createDirectories()

        def allFiles(): List[File] = sourcedir.glob("**.adoc").toList

        def allPosts(): List[Post] = allFiles().map { f: File =>
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

        val imagedir = sourcedir./("images")
        if (imagedir.isDirectory) imagedir.copyTo(postdir./("images"), overwrite = true)
    }
  }


  def main(args: Array[String]): Unit = run(args: _*)

  def run(args: String*) = {
    command.parse(args) match {
      case Left(help)    =>
        println(help)
        sys.exit(0)
      case Right(result) => result
    }

  }
}

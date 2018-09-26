package vitzen


import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files._
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import org.webjars.WebJarAssetLocator
import vitzen.docparser.{AsciiDoctorImpl, AsciiMedicImpl, Post}


object Vitzen {

  val resourceLocator = new WebJarAssetLocator()
  def resourceBytes(path: String): Iterator[Byte] =
    try {
      Resource.getAsStream(resourceLocator.getFullPath(path)).buffered.bytes
    } catch {
      case e: IllegalArgumentException =>
        (File.currentWorkingDirectory / "vitzen/target/web/sass/main/stylesheets/" / path).bytes
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


        sourcedir./("images").copyTo(postdir./("images"), overwrite = true)
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
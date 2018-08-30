package vitzen


import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files.{File, InputStreamOps, Resource}
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import org.asciidoctor.Asciidoctor
import org.webjars.WebJarAssetLocator


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

        val resourceLocator = new WebJarAssetLocator()

        (targetdir/"vitzen.css").writeBytes(
          Resource.getAsStream(resourceLocator.getFullPath("vitzen.css")).buffered.bytes)
        (targetdir/"highlight.js").writeBytes(
          Resource.getAsStream(resourceLocator.getFullPath("highlight.pack.js")).buffered.bytes)
//        write.over(ammPath(targetdir), read! resource/RelPath(resourceLocator.getFullPath()))


        lazy val asciidoctor: Asciidoctor = Asciidoctor.Factory.create()
        lazy val asciiData: AsciiData = new AsciiData(asciidoctor, sourcedir.path)

        val postdir = targetdir/"posts"
        postdir.createDirectories()

        val posts = asciiData.allPosts
        println(s"found ${posts.size} posts")

        println(s"posting to $targetdir")

        for (post <- posts) {
          println(post.title)
          val targetPath = postdir/post.targetPath()
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
      case Left(help) =>
        println(help)
        sys.exit(0)
      case Right(result) => result
    }

  }
}
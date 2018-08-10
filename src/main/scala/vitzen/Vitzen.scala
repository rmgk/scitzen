package vitzen


import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import ammonite.ops.{Path => ammPath, _}
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import org.asciidoctor.Asciidoctor
import org.webjars.WebJarAssetLocator


object Vitzen {


  val optSource = Opts.option[Path]("source", short = "s", metavar = "directory",
                                    help = "Directory containing Asciidoc source posts")
  val optTraget = Opts.option[Path]("output", short = "o", metavar = "directory",
                                    help = "Target output directory")

  val command = Command(name = "vitzen", header = "Convert Asciidoc documents into a webpage format.") {
    (optSource, optTraget).mapN {
      (sourcedir, targetdir) =>

        println(s"processing $sourcedir")

        val resourceLocator = new WebJarAssetLocator()

        write.over(ammPath(targetdir)/"vitzen.css", read.bytes(resource()/RelPath(resourceLocator.getFullPath("vitzen.css"))))
        write.over(ammPath(targetdir)/"highlight.js", read! resource/RelPath(resourceLocator.getFullPath("highlight.pack.js")))


        lazy val asciidoctor: Asciidoctor = Asciidoctor.Factory.create()
        lazy val asciiData: AsciiData = new AsciiData(asciidoctor, sourcedir)

        val postdir = targetdir.resolve("posts")
        Files.createDirectories(postdir)


        val posts = asciiData.allPosts
        println(s"found ${posts.size} posts")

        println(s"posting to $targetdir")

        for (post <- posts) {
          println(post.title)
          write.over(ammPath(postdir.resolve(post.targetPath())), Pages("../").makePostHtml(post))
        }

        write.over(ammPath(targetdir.resolve("index.html")), Pages().makeIndexOf(posts))
        Files.copy(Paths.get("target/web/sass/main/stylesheets/vitzen.css"),
                   targetdir.resolve("vitzen.css"),
                   StandardCopyOption.REPLACE_EXISTING)


        cp.over(ammPath(sourcedir.resolve("images")), ammPath(postdir.resolve("images")))
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
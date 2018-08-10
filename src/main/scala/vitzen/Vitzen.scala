package vitzen


import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import cats.implicits._
import com.monovore.decline.{Command, Opts}
import org.asciidoctor.Asciidoctor

object Vitzen {


  val optSource = Opts.option[Path]("source", short = "s", metavar = "directory",
                                    help = "Directory containing Asciidoc source posts")
  val optTraget = Opts.option[Path]("output", short = "o", metavar = "directory",
                                    help = "Target output directory")

  val command = Command(name = "vitzen", header = "Convert Asciidoc documents into a webpage format.") {
    (optSource, optTraget).mapN {
      (sourcedir, targetdir) =>

        println(s"processing $sourcedir")


        lazy val asciidoctor: Asciidoctor = Asciidoctor.Factory.create()
        lazy val asciiData: AsciiData = new AsciiData(asciidoctor, sourcedir)

        lazy val vitzen = new VitzenPages(asciiData, sourcedir)


        val postdir = targetdir.resolve("posts")
        Files.createDirectories(postdir)


        val posts = asciiData.allPosts
        println(s"found ${posts.size} posts")

        println(s"posting to $targetdir")

        def write(path: Path, content: String) = {
          Files.write(path, content.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE)
        }

        for (post <- posts) {
          println(post.title)
          write(postdir.resolve(post.targetPath()), vitzen.getContent(post))
        }

        write(targetdir.resolve("index.html"), vitzen.archive())
        Files.copy(Paths.get("target/web/sass/main/stylesheets/vitzen.css"), targetdir.resolve("vitzen.css"))

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
package vitzen


import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

object Vitzen {
  def main(args: Array[String]): Unit = {

/// in services

    import org.asciidoctor.Asciidoctor

    val postsdir = Paths.get("../Tagebuch/posts")
    println(s"processing $postsdir")


/* ====== asciidoctor ====== */

    lazy val asciidoctor: Asciidoctor = Asciidoctor.Factory.create()
    lazy val asciiData: AsciiData = new AsciiData(asciidoctor, postsdir)

    lazy val vitzen = new VitzenPages(asciiData, postsdir)

    val targetdir = Paths.get("posts/")
    Files.createDirectories(targetdir)

    val posts = asciiData.allPosts
    println(s"found ${posts.size} posts")

    println(s"posting to $targetdir")

    def write(path: Path, content: String) = {
      Files.write(path, content.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE)
    }

    for (post <- posts) {
      println(post.title)
      write(targetdir.resolve(post.targetPath()), vitzen.getContent(post))
    }

    write(Paths.get("index.html"), vitzen.archive())

/// in server

//pathPrefix("vitzen") {
//  path("") {
//    complete(vitzen.archive())
//  }~
//  path("posts" / Segments) { name =>
//    val path = name.filter(_ != "..").mkString("/")
//    if (path.endsWith("adoc"))
//      complete(vitzen.getContent(path))
//    else
//      getFromFile(postsPath.resolve(path).toFile)
//  }
//}
  }
}
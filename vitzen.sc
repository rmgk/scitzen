import $ivy.`org.jruby:jruby-complete:9.1.16.0`
import $ivy.`org.asciidoctor:asciidoctorj:1.5.6`
import $file.lib.AsciiData
import $file.lib.VitzenPages
import VitzenPages._
import AsciiData._
import java.nio.file.{Path, Paths}
import ammonite.ops._


/// in services

import org.asciidoctor.Asciidoctor

val postsdir = Paths.get("/home/ragnar/Sync/Tagebuch/posts")

/* ====== asciidoctor ====== */

lazy val asciidoctor: Asciidoctor = Asciidoctor.Factory.create()
lazy val asciiData: AsciiData = new AsciiData(asciidoctor, postsdir)

lazy val vitzen = new VitzenPages(asciiData, postsdir)

for (post <- asciiData.allPosts) {
  write.over(pwd/'posts/RelPath(post.targetPath()), vitzen.getContent(post))
}

write.over(pwd/"index.html", vitzen.archive())

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

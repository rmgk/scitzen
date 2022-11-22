import Dependencies._
import Settings._

val graalVersion = "22.3.0"

lazy val scitzen = project.in(file("."))
  // .enablePlugins(NativeImagePlugin)
  .settings(
    name         := "scitzen",
    organization := "de.rmgk",
    scalaVersion_3,
    jitpackResolver,
    libraryDependencies ++= Seq(
      jsoup.value,
      directories.value,
      upickle.value,
      scalatags.value,
      slips.chain.value,
      slips.logging.value,
      slips.scip.value,
      slips.options.value,
      scopt.value,
      "org.webjars.bowergithub.prismjs" % "prism"         % "1.28.0",
      "org.webjars.npm"                 % "katex"         % "0.16.0",
      "org.jbibtex"                     % "jbibtex"       % "1.0.20",
      "de.undercouch"                   % "citeproc-java" % "2.0.0",
      betterFiles.value.cross(CrossVersion.for3Use2_13),
      munit.value,
      "org.graalvm.js" % "js" % graalVersion, // explicitly depend on graal.js to allow running on non-graal JVMs
    ),
    libraryDependencies ++= jsoniterScalaAll.value,
    libraryDependencySchemes += "com.lihaoyi" %% "geny" % VersionScheme.Always,
    Compile / run / fork := true,
    TaskKey[Unit]("writeClasspath", "writes the classpath to a file in the target dir") := {
      val cp         = (Compile / fullClasspathAsJars).value
      val cpstring   = cp.map(at => s"""-cp "${at.data}"\n""").mkString("")
      val targetpath = target.value.toPath.resolve("classpath.txt")
      IO.write(targetpath.toFile, cpstring)
      ()
    }
  )

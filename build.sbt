import Dependencies._
import Settings._

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
      "org.webjars.bowergithub.prismjs" % "prism"         % "1.29.0",
      "org.webjars.npm"                 % "katex"         % "0.16.4",
      "org.jbibtex"                     % "jbibtex"       % "1.0.20",
      "de.undercouch"                   % "citeproc-java" % "2.0.0",
      betterFiles.value.cross(CrossVersion.for3Use2_13),
      munit.value,
      "org.graalvm.js" % "js" % "22.3.1", // explicitly depend on graal.js to allow running on non-graal JVMs
    ),
    libraryDependencies ++= jsoniterScalaAll.value,
    libraryDependencySchemes += "com.lihaoyi" %% "geny" % VersionScheme.Always,
    Compile / run / fork := true
  )

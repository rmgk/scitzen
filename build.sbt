import Dependencies._
import Settings._

val graalVersion = "22.1.0"

lazy val scitzen = project.in(file("."))
  .enablePlugins(NativeImagePlugin)
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
    // javaOptions += "-agentlib:native-image-agent=config-output-dir=src/main/resources/META-INF/native-image",
    Compile / run / fork := true,
    nativeImageVersion   := graalVersion,
    nativeImageJvm       := "graalvm-java17",
    nativeImageOptions ++= Seq(
      "--no-fallback",
      // "--initialize-at-build-time",
      // "--initialize-at-run-time=scala.util.Random",
      "-J-Xmx7G",
      "--language:js",
      "-H:+ReportExceptionStackTraces",
      "-H:IncludeResources=META-INF/resources/webjars/prism/components/.*.min.js$",
      "-H:IncludeResources=META-INF/resources/webjars/prism/themes/.*.css$",
      "-H:IncludeResources=META-INF/resources/webjars/katex/.*/dist/katex.min.js$",
      "-H:IncludeResources=scitzen.css",
    ),
  )

// fix some linting nonsense
Global / excludeLintKeys += nativeImageVersion
Global / excludeLintKeys += nativeImageJvm

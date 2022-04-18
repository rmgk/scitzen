import Dependencies.*
import Settings.*

lazy val root = project.in(file(".")).aggregate(parser, scitzen)
  .settings(
    publishArtifact := false,
    name            := "scitzen-root",
    organization    := "de.rmgk"
  )

lazy val parser = project.in(file("parser"))
  .settings(
    name         := "scitzen-parser",
    organization := "de.rmgk",
    scalaVersion_213,
    strictCompile,
    libraryDependencies ++= Seq(
      betterFiles.value,
      fastparse.value.exclude("com.lihaoyi", "geny_2.13"),
    ),
    libraryDependencies ++= jsoniterScalaAll.value,
  )

val graalVersion = "22.0.0.2"

lazy val scitzen = project.in(file("cli"))
  .enablePlugins(NativeImagePlugin)
  .dependsOn(parser)
  .settings(
    name         := "scitzen",
    organization := "de.rmgk",
    scalaVersion_3,
    strictCompile,
    libraryDependencies ++= Seq(
      decline.value,
      jsoup.value,
      upickle.value.exclude("com.lihaoyi", "sourcecode_3"),
      scalatags.value.exclude("com.lihaoyi", "sourcecode_3"),
      "org.webjars.bowergithub.prismjs" % "prism"         % "1.27.0",
      "org.webjars.npm"                 % "katex"         % "0.15.1",
      "org.typelevel"                 %%% "cats-parse"    % "0.3.7",
      "org.jbibtex"                     % "jbibtex"       % "1.0.20",
      "de.undercouch"                   % "citeproc-java" % "2.0.0",
      // explicitly depend on graal.js to allow running on non-graal JVMs
      "org.graalvm.js" % "js" % graalVersion,
    ),
    javaOptions += "-agentlib:native-image-agent=config-output-dir=src/main/resources/META-INF/native-image",
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

lazy val benchmarks = project.in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .dependsOn(scitzen)
  .settings(
    name         := "scitzen-benchmarks",
    organization := "de.rmgk",
    scalaVersion_3,
    strictCompile,
  )

// fix some linting nonsense
Global / excludeLintKeys += nativeImageVersion
Global / excludeLintKeys += nativeImageJvm

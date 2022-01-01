import Dependencies.*
import Settings.*
import org.irundaia.sass.Maxified

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
      fastparse.value,
    ),
    libraryDependencies ++= jsoniterScalaAll.value,
  )

lazy val scitzen = project.in(file("cli"))
  .enablePlugins(NativeImagePlugin)
  .enablePlugins(SbtSassify)
  .dependsOn(parser)
  .settings(
    name         := "scitzen",
    organization := "de.rmgk",
    scalaVersion_3,
    Compile / resources ++= (Assets / SassKeys.sassify).value,
    strictCompile,
    libraryDependencies ++= Seq(
      decline.value,
      scalatags.value.cross(CrossVersion.for3Use2_13),
      normalizecss.value,
      "org.webjars.bowergithub.prismjs" % "prism"         % "1.25.0",
      "org.webjars.npm"                 % "katex"         % "0.15.1",
      "org.typelevel"                 %%% "cats-parse"    % "0.3.6",
      "org.jbibtex"                     % "jbibtex"       % "1.0.19",
      ("de.undercouch"                  % "citeproc-java" % "2.0.0")
        .exclude("org.graalvm.js", "js")
        .exclude("org.graalvm.sdk", "graal-sdk")
    ),
    // libraryDependencies ++= jsoniterScalaAll.value,
    SassKeys.cssStyle  := Maxified,
    nativeImageVersion := "21.3.0",
    nativeImageJvm     := "graalvm-java17",
    nativeImageOptions ++= Seq(
      "--no-fallback",
      "--no-server",
      "--initialize-at-build-time",
      "--initialize-at-run-time=scala.util.Random",
      "-J-Xmx24G",
      "--language:js",
      "-H:+ReportExceptionStackTraces",
      "-H:IncludeResources='META-INF/resources/webjars/prism/components/.*.min.js$'",
      "-H:IncludeResources='META-INF/resources/webjars/prism/themes/.*.css$'",
      "-H:IncludeResources='META-INF/resources/webjars/katex/.*/dist/katex.min.js$'",
    ),
    (Compile / resources) += CustomUtil.fetchResource(
      "https://cdn.jsdelivr.net/npm/katex@0.15.1/dist/katex.min.js",
      "60bf7b560459a4af660e5cd9c4350d7766e9de63",
      (Compile / managedResourceDirectories).value.head.toPath.resolve("scitzen/katex.min.js")
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

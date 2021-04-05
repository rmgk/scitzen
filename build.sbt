import Dependencies._
import Settings._
import org.irundaia.sass.Maxified

lazy val scitzen = project.in(file("."))
  .enablePlugins(NativeImagePlugin)
  .enablePlugins(SbtSassify)
  .settings(
    name := "scitzen",
    organization := "de.rmgk",
    scalaVersion_213,
    Compile / resources ++= (Assets / SassKeys.sassify).value,
    strictCompile,
    libraryDependencies ++= Seq(
      decline.value,
      betterFiles.value,
      scalatags.value,
      fastparse.value,
      scalatest.value,
      scalacheck.value,
      pprint.value,
      catsCore.value,
      scribe.value,
      normalizecss.value,
      tomlScala.value,
    ),
    libraryDependencies ++= jsoniterScalaAll.value,
    SassKeys.cssStyle := Maxified,
    nativeImageVersion := "21.0.0",
    nativeImageOptions ++= Seq(
      "--initialize-at-build-time",
      "--no-fallback",
      "--no-server"
    ),
  )

// fix some linting nonsense
Global / excludeLintKeys += nativeImageVersion

import Dependencies.Compile._
import Dependencies.Test._
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
      cats.value,
      scribe.value,
      normalizecss.value,
      tomlScala.value,
    ),
    jsoniterScala,
    SassKeys.cssStyle := Maxified,
    nativeImageVersion := "20.3.0",
    nativeImageOptions ++= Seq(
      "--initialize-at-build-time",
      "--no-fallback",
      "--no-server"
    ),
    libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value))
  )

// fix some linting nonsense
Global / excludeLintKeys += nativeImageVersion

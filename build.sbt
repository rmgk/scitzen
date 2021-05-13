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
    nativeImageVersion := "21.1.0",
    nativeImageJvm := "graalvm-java11",
    nativeImageOptions ++= Seq(
      "--no-fallback",
      "--no-server",
      "--initialize-at-build-time",
      "--initialize-at-run-time=scala.util.Random"
    ),
  )

// fix some linting nonsense
Global / excludeLintKeys += nativeImageVersion
Global / excludeLintKeys += nativeImageJvm

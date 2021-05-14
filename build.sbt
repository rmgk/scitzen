import Dependencies._
import Settings._
import org.irundaia.sass.Maxified

lazy val parser = project.in(file("parser"))
  .settings(
    name := "scitzen-parser",
    organization := "de.rmgk",
    scalaVersion_213,
    strictCompile,
    libraryDependencies ++= Seq(
      betterFiles.value,
      fastparse.value,
      catsCore.value,
    ),
  )

lazy val scitzen = project.in(file("."))
  .enablePlugins(NativeImagePlugin)
  .enablePlugins(SbtSassify)
  .dependsOn(parser)
  .settings(
    name := "scitzen",
    organization := "de.rmgk",
    scalaVersion_3,
    Compile / resources ++= (Assets / SassKeys.sassify).value,
    strictCompile,
    scalacOptions ++= List("-rewrite", "-source", "3.0-migration"),
    libraryDependencies ++= Seq(
      decline.value.cross(CrossVersion.for3Use2_13),
      betterFiles.value.cross(CrossVersion.for3Use2_13),
      scalatags.value.cross(CrossVersion.for3Use2_13),
      scalatest.value.cross(CrossVersion.for3Use2_13),
      scalacheck.value,
      catsCore.value.cross(CrossVersion.for3Use2_13),
      normalizecss.value,
      tomlScala.value.cross(CrossVersion.for3Use2_13),
    ),
    // libraryDependencies ++= jsoniterScalaAll.value,
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

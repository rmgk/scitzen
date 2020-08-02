import Dependencies._
import Settings._
import org.irundaia.sass.Maxified

lazy val scitzen = project.in(file("."))
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(GraalVMNativeImagePlugin)
  .enablePlugins(SbtSassify)
  .settings(
    name := "scitzen",
    organization := "de.rmgk",
    scalaVersion_213,
    Compile / resources ++= (Assets / SassKeys.sassify).value,
    strictCompile,
    decline,
    betterFiles,
    scalatags,
    fastparse,
    scalatest,
    scalacheck,
    pprint,
    cats,
    jsoniter,
    scribe,
    SassKeys.cssStyle := Maxified,
    normalizecss,
    graalVMNativeImageOptions ++= Seq(
      "--initialize-at-build-time",
      "--no-fallback",
      "--no-server"
    ),
    toml,
    libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value))
  )

lazy val nativeImage = taskKey[File]("calls graalvm native image")

nativeImage := (scitzen / GraalVMNativeImage / packageBin).value

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
                     resolvers += Resolver.sonatypeRepo("public"),
                     libraryDependencies ++= Seq(
                       "com.vdurmont" % "emoji-java" % "5.1.1"
                       ),
                     strictCompile,
                     decline,
                     betterFiles,
                     scalatags,
                     strictCompile,
                     fastparse,
                     scalatest,
                     scalacheck,
                     pprint,
                     cats,
                     upickle,
                     scribe,
                     SassKeys.cssStyle := Maxified,
                     normalizecss,
                     graalVMNativeImageOptions += "--initialize-at-build-time",
                     toml,
                     jsoup
                     )

bloopSources

lazy val nativeImage = taskKey[File]("calls graalvm native image")

nativeImage := (scitzen / GraalVMNativeImage / packageBin).value

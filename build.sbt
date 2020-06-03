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
                     Resolvers.bintrayPublish("rmgk", "rmgk", "scitzen"),
                     libraryDependencies ++= Seq(
                       "com.vdurmont" % "emoji-java" % "5.1.1"
                       ),
                     strictCompile,
                     decline,
                     betterFiles,
                     scalatags,
                     fastparse,
                     scalatest,
                     scalacheck,
                     pprint,
                     cats,
                     upickle,
                     scribe,
                     SassKeys.cssStyle := Maxified,
                     normalizecss,
                     graalVMNativeImageOptions ++= Seq(
                       "--initialize-at-build-time",
                       "--no-fallback"
                     ),
                     toml,
                     jsoup,
                     libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value))
                     )

lazy val nativeImage = taskKey[File]("calls graalvm native image")

nativeImage := (scitzen / GraalVMNativeImage / packageBin).value
